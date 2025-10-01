#!/usr/bin/env python3
"""
Train fraud detection models for health insurance claims.
"""

import pandas as pd
import numpy as np
import joblib
import os
from datetime import datetime
from sklearn.model_selection import train_test_split, cross_val_score, GridSearchCV
from sklearn.preprocessing import StandardScaler, LabelEncoder
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    classification_report, confusion_matrix, roc_auc_score,
    precision_recall_curve, average_precision_score
)
from imblearn.over_sampling import SMOTE
import warnings
warnings.filterwarnings('ignore')

def load_and_prepare_data():
    """Load and prepare the claims data for modeling."""
    print("Loading data...")
    claims_df = pd.read_csv('data/synthetic-claims.csv', comment='#')
    
    # Convert date columns
    claims_df['service_date'] = pd.to_datetime(claims_df['service_date'])
    claims_df['submission_date'] = pd.to_datetime(claims_df['submission_date'])
    
    print(f"Loaded {len(claims_df)} claims")
    print(f"Fraud rate: {claims_df['is_fraud'].mean():.2%}")
    
    return claims_df

def engineer_features(df):
    """Create features for modeling."""
    print("Engineering features...")
    
    # Create copy for modeling
    model_df = df.copy()
    
    # Encode categorical variables
    categorical_columns = ['claim_type', 'place_of_service', 'specialty', 
                          'provider_type', 'region', 'gender', 'state', 'plan_type']
    
    label_encoders = {}
    for col in categorical_columns:
        le = LabelEncoder()
        model_df[f'{col}_encoded'] = le.fit_transform(model_df[col])
        label_encoders[col] = le
    
    # Create additional features
    model_df['amount_ratio'] = model_df['billed_amount'] / (model_df['allowed_amount'] + 1)
    model_df['payment_ratio'] = model_df['paid_amount'] / (model_df['billed_amount'] + 1)
    model_df['procedures_per_diagnosis'] = model_df['num_procedures'] / (model_df['num_diagnosis_codes'] + 1)
    
    # Provider-level aggregated features
    provider_stats = model_df.groupby('provider_id').agg({
        'billed_amount': ['mean', 'std'],
        'days_to_submit': 'mean',
        'num_procedures': 'mean'
    }).fillna(0)
    provider_stats.columns = ['provider_avg_billed', 'provider_std_billed', 
                              'provider_avg_submit_days', 'provider_avg_procedures']
    
    model_df = model_df.merge(provider_stats, left_on='provider_id', right_index=True, how='left')
    
    # Member-level aggregated features
    member_stats = model_df.groupby('member_id').agg({
        'claim_id': 'count',
        'billed_amount': 'mean'
    })
    member_stats.columns = ['member_claim_count', 'member_avg_billed']
    
    model_df = model_df.merge(member_stats, left_on='member_id', right_index=True, how='left')
    
    # Select features for modeling
    feature_columns = [
        'billed_amount', 'allowed_amount', 'paid_amount', 'days_to_submit',
        'num_diagnosis_codes', 'num_procedures', 'member_age', 'chronic_conditions',
        'amount_ratio', 'payment_ratio', 'procedures_per_diagnosis',
        'provider_avg_billed', 'provider_std_billed', 'provider_avg_submit_days',
        'provider_avg_procedures', 'member_claim_count', 'member_avg_billed'
    ] + [f'{col}_encoded' for col in categorical_columns]
    
    X = model_df[feature_columns]
    y = model_df['is_fraud'].astype(int)
    
    print(f"Created {len(feature_columns)} features")
    
    return X, y, feature_columns, label_encoders

def train_models(X_train, X_test, y_train, y_test, feature_columns):
    """Train multiple models and compare performance."""
    print("\nTraining models...")
    
    # Handle class imbalance
    smote = SMOTE(random_state=42)
    X_train_balanced, y_train_balanced = smote.fit_resample(X_train, y_train)
    
    # Scale features
    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train_balanced)
    X_test_scaled = scaler.transform(X_test)
    
    models = {
        'Random Forest': RandomForestClassifier(
            n_estimators=100,
            max_depth=10,
            min_samples_split=20,
            min_samples_leaf=10,
            random_state=42,
            n_jobs=-1
        ),
        'Gradient Boosting': GradientBoostingClassifier(
            n_estimators=100,
            max_depth=5,
            learning_rate=0.1,
            random_state=42
        ),
        'Logistic Regression': LogisticRegression(
            max_iter=1000,
            random_state=42
        )
    }
    
    results = {}
    
    for name, model in models.items():
        print(f"\nTraining {name}...")
        
        # Train model
        model.fit(X_train_scaled, y_train_balanced)
        
        # Make predictions
        y_pred = model.predict(X_test_scaled)
        y_pred_proba = model.predict_proba(X_test_scaled)[:, 1]
        
        # Calculate metrics
        auc_score = roc_auc_score(y_test, y_pred_proba)
        avg_precision = average_precision_score(y_test, y_pred_proba)
        
        # Cross-validation score
        cv_scores = cross_val_score(model, X_train_scaled, y_train_balanced, 
                                   cv=5, scoring='roc_auc')
        
        results[name] = {
            'model': model,
            'auc': auc_score,
            'avg_precision': avg_precision,
            'cv_mean': cv_scores.mean(),
            'cv_std': cv_scores.std(),
            'predictions': y_pred,
            'probabilities': y_pred_proba,
            'classification_report': classification_report(y_test, y_pred, 
                                                          target_names=['Normal', 'Fraud'])
        }
        
        print(f"AUC: {auc_score:.3f}")
        print(f"Average Precision: {avg_precision:.3f}")
        print(f"CV Score: {cv_scores.mean():.3f} (+/- {cv_scores.std():.3f})")
    
    # Select best model based on AUC
    best_model_name = max(results, key=lambda x: results[x]['auc'])
    best_model = results[best_model_name]['model']
    
    print(f"\nBest model: {best_model_name} (AUC: {results[best_model_name]['auc']:.3f})")
    
    # Feature importance for tree-based models
    if hasattr(best_model, 'feature_importances_'):
        feature_importance = pd.DataFrame({
            'feature': feature_columns,
            'importance': best_model.feature_importances_
        }).sort_values('importance', ascending=False)
        
        print("\nTop 10 Most Important Features:")
        print(feature_importance.head(10).to_string(index=False))
    
    return best_model, scaler, results

def save_models(model, scaler, label_encoders, feature_columns):
    """Save trained models and preprocessing objects."""
    print("\nSaving models...")
    
    # Create models directory if it doesn't exist
    os.makedirs('ml/models', exist_ok=True)
    
    # Save model
    joblib.dump(model, 'ml/models/fraud_detection_model.pkl')
    print("Saved model to ml/models/fraud_detection_model.pkl")
    
    # Save scaler
    joblib.dump(scaler, 'ml/models/scaler.pkl')
    print("Saved scaler to ml/models/scaler.pkl")
    
    # Save label encoders
    joblib.dump(label_encoders, 'ml/models/label_encoders.pkl')
    print("Saved label encoders to ml/models/label_encoders.pkl")
    
    # Save feature columns
    joblib.dump(feature_columns, 'ml/models/feature_columns.pkl')
    print("Saved feature columns to ml/models/feature_columns.pkl")
    
    # Save model metadata
    metadata = {
        'training_date': datetime.now().isoformat(),
        'model_type': type(model).__name__,
        'n_features': len(feature_columns),
        'feature_names': feature_columns
    }
    joblib.dump(metadata, 'ml/models/model_metadata.pkl')
    print("Saved model metadata")

def main():
    """Main training pipeline."""
    print("=" * 50)
    print("Fraud Detection Model Training")
    print("=" * 50)
    
    # Load and prepare data
    df = load_and_prepare_data()
    
    # Engineer features
    X, y, feature_columns, label_encoders = engineer_features(df)
    
    # Split data
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42, stratify=y
    )
    print(f"\nTraining set: {len(X_train)} samples")
    print(f"Test set: {len(X_test)} samples")
    
    # Train models
    best_model, scaler, results = train_models(X_train, X_test, y_train, y_test, feature_columns)
    
    # Save models
    save_models(best_model, scaler, label_encoders, feature_columns)
    
    print("\n" + "=" * 50)
    print("Training Complete!")
    print("=" * 50)
    
    # Print final summary
    print("\nModel Performance Summary:")
    for name, result in results.items():
        print(f"\n{name}:")
        print(f"  AUC: {result['auc']:.3f}")
        print(f"  Avg Precision: {result['avg_precision']:.3f}")
        print(f"  CV Score: {result['cv_mean']:.3f} (+/- {result['cv_std']:.3f})")

if __name__ == "__main__":
    main()