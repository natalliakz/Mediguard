#!/usr/bin/env python3
"""
Utility functions for fraud detection models.
"""

import pandas as pd
import numpy as np
import joblib
import os
from typing import Dict, List, Tuple, Any

class FraudDetector:
    """Fraud detection model wrapper for easy inference."""
    
    def __init__(self, model_dir: str = 'ml/models'):
        """Initialize the fraud detector."""
        self.model_dir = model_dir
        self.model = None
        self.scaler = None
        self.label_encoders = None
        self.feature_columns = None
        self.metadata = None
        self.load_models()
    
    def load_models(self):
        """Load trained models and preprocessing objects."""
        try:
            self.model = joblib.load(os.path.join(self.model_dir, 'fraud_detection_model.pkl'))
            self.scaler = joblib.load(os.path.join(self.model_dir, 'scaler.pkl'))
            self.label_encoders = joblib.load(os.path.join(self.model_dir, 'label_encoders.pkl'))
            self.feature_columns = joblib.load(os.path.join(self.model_dir, 'feature_columns.pkl'))
            self.metadata = joblib.load(os.path.join(self.model_dir, 'model_metadata.pkl'))
            print("Models loaded successfully")
        except Exception as e:
            print(f"Error loading models: {e}")
            raise
    
    def prepare_claim(self, claim_data: Dict[str, Any]) -> np.ndarray:
        """Prepare a single claim for prediction."""
        # Create DataFrame from claim data
        df = pd.DataFrame([claim_data])
        
        # Encode categorical variables
        for col, encoder in self.label_encoders.items():
            if col in df.columns:
                # Handle unknown categories
                df[f'{col}_encoded'] = df[col].apply(
                    lambda x: encoder.transform([x])[0] 
                    if x in encoder.classes_ else -1
                )
        
        # Create derived features
        df['amount_ratio'] = df['billed_amount'] / (df['allowed_amount'] + 1)
        df['payment_ratio'] = df['paid_amount'] / (df['billed_amount'] + 1)
        df['procedures_per_diagnosis'] = df['num_procedures'] / (df['num_diagnosis_codes'] + 1)
        
        # Add default values for aggregated features if not present
        agg_features = ['provider_avg_billed', 'provider_std_billed', 
                       'provider_avg_submit_days', 'provider_avg_procedures',
                       'member_claim_count', 'member_avg_billed']
        
        for feature in agg_features:
            if feature not in df.columns:
                df[feature] = 0
        
        # Select and order features
        features = df[self.feature_columns].fillna(0)
        
        # Scale features
        features_scaled = self.scaler.transform(features)
        
        return features_scaled
    
    def predict_fraud(self, claim_data: Dict[str, Any]) -> Dict[str, Any]:
        """Predict fraud probability for a single claim."""
        # Prepare features
        features = self.prepare_claim(claim_data)
        
        # Make prediction
        fraud_prob = self.model.predict_proba(features)[0, 1]
        is_fraud = self.model.predict(features)[0]
        
        # Determine risk level
        if fraud_prob < 0.2:
            risk_level = 'Low'
        elif fraud_prob < 0.5:
            risk_level = 'Medium'
        elif fraud_prob < 0.8:
            risk_level = 'High'
        else:
            risk_level = 'Critical'
        
        return {
            'fraud_probability': float(fraud_prob),
            'is_fraud': bool(is_fraud),
            'risk_level': risk_level,
            'claim_id': claim_data.get('claim_id', 'Unknown')
        }
    
    def batch_predict(self, claims_df: pd.DataFrame) -> pd.DataFrame:
        """Predict fraud for multiple claims."""
        predictions = []
        
        for _, row in claims_df.iterrows():
            claim_data = row.to_dict()
            prediction = self.predict_fraud(claim_data)
            predictions.append(prediction)
        
        return pd.DataFrame(predictions)
    
    def get_model_info(self) -> Dict[str, Any]:
        """Get information about the loaded model."""
        return {
            'model_type': self.metadata.get('model_type', 'Unknown'),
            'training_date': self.metadata.get('training_date', 'Unknown'),
            'n_features': self.metadata.get('n_features', 0),
            'feature_names': self.metadata.get('feature_names', [])
        }

def calculate_fraud_metrics(claims_df: pd.DataFrame) -> Dict[str, Any]:
    """Calculate fraud detection metrics for a dataset."""
    total_claims = len(claims_df)
    fraud_claims = claims_df['is_fraud'].sum()
    fraud_rate = fraud_claims / total_claims if total_claims > 0 else 0
    
    total_amount = claims_df['billed_amount'].sum()
    fraud_amount = claims_df[claims_df['is_fraud'] == True]['billed_amount'].sum()
    fraud_amount_rate = fraud_amount / total_amount if total_amount > 0 else 0
    
    return {
        'total_claims': int(total_claims),
        'fraud_claims': int(fraud_claims),
        'fraud_rate': float(fraud_rate),
        'total_amount': float(total_amount),
        'fraud_amount': float(fraud_amount),
        'fraud_amount_rate': float(fraud_amount_rate),
        'avg_claim_amount': float(claims_df['billed_amount'].mean()),
        'avg_fraud_amount': float(claims_df[claims_df['is_fraud'] == True]['billed_amount'].mean())
        if fraud_claims > 0 else 0
    }

def identify_high_risk_providers(claims_df: pd.DataFrame, threshold: float = 0.1) -> pd.DataFrame:
    """Identify providers with high fraud rates."""
    provider_stats = claims_df.groupby('provider_id').agg({
        'claim_id': 'count',
        'is_fraud': ['sum', 'mean'],
        'billed_amount': ['mean', 'std']
    })
    
    provider_stats.columns = ['_'.join(col).strip() for col in provider_stats.columns.values]
    provider_stats = provider_stats.rename(columns={
        'claim_id_count': 'total_claims',
        'is_fraud_sum': 'fraud_claims',
        'is_fraud_mean': 'fraud_rate',
        'billed_amount_mean': 'avg_billed',
        'billed_amount_std': 'std_billed'
    })
    
    # Filter high-risk providers
    high_risk = provider_stats[provider_stats['fraud_rate'] > threshold]
    high_risk = high_risk.sort_values('fraud_rate', ascending=False)
    
    return high_risk

def generate_fraud_report(claims_df: pd.DataFrame) -> str:
    """Generate a text report of fraud analysis."""
    metrics = calculate_fraud_metrics(claims_df)
    high_risk_providers = identify_high_risk_providers(claims_df)
    
    report = f"""
    FRAUD DETECTION ANALYSIS REPORT
    ================================
    
    Overall Metrics:
    - Total Claims: {metrics['total_claims']:,}
    - Fraudulent Claims: {metrics['fraud_claims']:,}
    - Fraud Rate: {metrics['fraud_rate']:.2%}
    - Total Billed Amount: ${metrics['total_amount']:,.2f}
    - Fraudulent Amount: ${metrics['fraud_amount']:,.2f}
    - Fraud Amount Rate: {metrics['fraud_amount_rate']:.2%}
    
    Claim Amounts:
    - Average Claim: ${metrics['avg_claim_amount']:,.2f}
    - Average Fraud Claim: ${metrics['avg_fraud_amount']:,.2f}
    
    High-Risk Providers:
    - Identified: {len(high_risk_providers)} providers
    - Combined Fraud Claims: {high_risk_providers['fraud_claims'].sum():,}
    - Average Fraud Rate: {high_risk_providers['fraud_rate'].mean():.2%}
    
    Top 5 High-Risk Providers:
    """
    
    for idx, (provider_id, row) in enumerate(high_risk_providers.head(5).iterrows(), 1):
        report += f"""
    {idx}. Provider {provider_id}:
       - Total Claims: {row['total_claims']:.0f}
       - Fraud Rate: {row['fraud_rate']:.2%}
       - Avg Billed: ${row['avg_billed']:,.2f}
        """
    
    return report

# Example usage
if __name__ == "__main__":
    # Load sample data
    claims_df = pd.read_csv('data/synthetic-claims.csv', comment='#')
    
    # Generate report
    report = generate_fraud_report(claims_df)
    print(report)
    
    # Test fraud detector (requires trained model)
    try:
        detector = FraudDetector()
        
        # Test single prediction
        sample_claim = claims_df.iloc[0].to_dict()
        prediction = detector.predict_fraud(sample_claim)
        print(f"\nSample Prediction:")
        print(f"Claim ID: {prediction['claim_id']}")
        print(f"Fraud Probability: {prediction['fraud_probability']:.2%}")
        print(f"Risk Level: {prediction['risk_level']}")
    except Exception as e:
        print(f"\nModel testing skipped: {e}")