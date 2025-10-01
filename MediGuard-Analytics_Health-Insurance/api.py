#!/usr/bin/env python3
"""
FastAPI application for health insurance fraud detection.
"""

from fastapi import FastAPI, HTTPException, Query
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel, Field
from typing import List, Dict, Any, Optional
from datetime import datetime, date
import pandas as pd
import numpy as np
import sys
import os

# Add ml directory to path
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from ml.model_utils import FraudDetector, calculate_fraud_metrics, identify_high_risk_providers

# Initialize FastAPI app
app = FastAPI(
    title="MediGuard Fraud Detection API",
    description="Health insurance claims fraud detection and risk assessment API",
    version="1.0.0",
    contact={
        "name": "MediGuard Analytics",
        "url": "https://www.mediguard-analytics.com",
        "email": "api@mediguard-analytics.com",
    },
)

# Enable CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Initialize fraud detector
try:
    fraud_detector = FraudDetector()
    model_loaded = True
except Exception as e:
    print(f"Warning: Could not load fraud detection model: {e}")
    model_loaded = False

# Pydantic models
class ClaimInput(BaseModel):
    """Input model for a single claim."""
    claim_id: str = Field(..., description="Unique claim identifier")
    member_id: str = Field(..., description="Member ID")
    provider_id: str = Field(..., description="Provider ID")
    service_date: str = Field(..., description="Date of service (YYYY-MM-DD)")
    submission_date: str = Field(..., description="Date of submission (YYYY-MM-DD)")
    billed_amount: float = Field(..., gt=0, description="Billed amount in USD")
    allowed_amount: float = Field(..., gt=0, description="Allowed amount in USD")
    paid_amount: float = Field(..., ge=0, description="Paid amount in USD")
    num_diagnosis_codes: int = Field(..., ge=1, description="Number of diagnosis codes")
    num_procedures: int = Field(..., ge=1, description="Number of procedures")
    member_age: int = Field(..., ge=0, le=120, description="Member age")
    chronic_conditions: int = Field(0, ge=0, description="Number of chronic conditions")
    claim_type: str = Field(..., description="Type of claim")
    place_of_service: str = Field(..., description="Place of service")
    specialty: str = Field(..., description="Provider specialty")
    provider_type: str = Field(..., description="Provider type")
    region: str = Field(..., description="Geographic region")
    gender: str = Field(..., description="Member gender (M/F)")
    state: str = Field(..., description="State code")
    plan_type: str = Field(..., description="Insurance plan type")

class FraudPrediction(BaseModel):
    """Output model for fraud prediction."""
    claim_id: str
    fraud_probability: float
    is_fraud: bool
    risk_level: str
    timestamp: datetime = Field(default_factory=datetime.now)

class BatchClaimInput(BaseModel):
    """Input model for batch claim processing."""
    claims: List[ClaimInput]

class ProviderRiskScore(BaseModel):
    """Output model for provider risk assessment."""
    provider_id: str
    total_claims: int
    fraud_claims: int
    fraud_rate: float
    avg_billed: float
    risk_score: float
    risk_level: str

class HealthCheckResponse(BaseModel):
    """Health check response model."""
    status: str
    model_loaded: bool
    timestamp: datetime
    version: str

class DataSample(BaseModel):
    """Sample data response model."""
    claim_id: str
    member_id: str
    provider_id: str
    service_date: str
    billed_amount: float
    is_fraud: bool
    fraud_type: str

class ModelInfo(BaseModel):
    """Model information response."""
    model_type: str
    training_date: str
    n_features: int
    feature_names: List[str]
    model_loaded: bool

# API Endpoints

@app.get("/", tags=["General"])
async def root():
    """Root endpoint with API information."""
    return {
        "name": "MediGuard Fraud Detection API",
        "version": "1.0.0",
        "documentation": "/docs",
        "health": "/health"
    }

@app.get("/health", response_model=HealthCheckResponse, tags=["General"])
async def health_check():
    """Check API health status."""
    return HealthCheckResponse(
        status="healthy",
        model_loaded=model_loaded,
        timestamp=datetime.now(),
        version="1.0.0"
    )

@app.get("/data", response_model=List[DataSample], tags=["Data"])
async def get_sample_data(
    limit: int = Query(10, ge=1, le=100, description="Number of samples to return"),
    fraud_only: bool = Query(False, description="Return only fraudulent claims")
):
    """Get sample claims data."""
    try:
        # Load sample data
        df = pd.read_csv('data/synthetic-claims.csv', comment='#')
        
        if fraud_only:
            df = df[df['is_fraud'] == True]
        
        # Sample and convert to response format
        sample = df.sample(min(limit, len(df)))
        
        return [
            DataSample(
                claim_id=row['claim_id'],
                member_id=row['member_id'],
                provider_id=row['provider_id'],
                service_date=row['service_date'],
                billed_amount=row['billed_amount'],
                is_fraud=row['is_fraud'],
                fraud_type=row['fraud_type']
            )
            for _, row in sample.iterrows()
        ]
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.post("/predict", response_model=FraudPrediction, tags=["Prediction"])
async def predict_fraud(claim: ClaimInput):
    """Predict fraud probability for a single claim."""
    if not model_loaded:
        raise HTTPException(
            status_code=503, 
            detail="Model not loaded. Please ensure model training is complete."
        )
    
    try:
        # Calculate days to submit
        service_date = datetime.strptime(claim.service_date, "%Y-%m-%d")
        submission_date = datetime.strptime(claim.submission_date, "%Y-%m-%d")
        days_to_submit = (submission_date - service_date).days
        
        # Prepare claim data
        claim_data = claim.dict()
        claim_data['days_to_submit'] = days_to_submit
        claim_data['primary_diagnosis'] = 'A10.0'  # Default diagnosis code
        claim_data['primary_procedure'] = '99213'  # Default procedure code
        
        # Get prediction
        prediction = fraud_detector.predict_fraud(claim_data)
        
        return FraudPrediction(**prediction)
        
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

@app.post("/predict/batch", response_model=List[FraudPrediction], tags=["Prediction"])
async def predict_fraud_batch(batch: BatchClaimInput):
    """Predict fraud probability for multiple claims."""
    if not model_loaded:
        raise HTTPException(
            status_code=503, 
            detail="Model not loaded. Please ensure model training is complete."
        )
    
    try:
        predictions = []
        
        for claim in batch.claims:
            # Calculate days to submit
            service_date = datetime.strptime(claim.service_date, "%Y-%m-%d")
            submission_date = datetime.strptime(claim.submission_date, "%Y-%m-%d")
            days_to_submit = (submission_date - service_date).days
            
            # Prepare claim data
            claim_data = claim.dict()
            claim_data['days_to_submit'] = days_to_submit
            claim_data['primary_diagnosis'] = 'A10.0'
            claim_data['primary_procedure'] = '99213'
            
            # Get prediction
            prediction = fraud_detector.predict_fraud(claim_data)
            predictions.append(FraudPrediction(**prediction))
        
        return predictions
        
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

@app.get("/model-info", response_model=ModelInfo, tags=["Model"])
async def get_model_info():
    """Get information about the loaded fraud detection model."""
    if not model_loaded:
        return ModelInfo(
            model_type="Not Loaded",
            training_date="N/A",
            n_features=0,
            feature_names=[],
            model_loaded=False
        )
    
    try:
        info = fraud_detector.get_model_info()
        return ModelInfo(
            model_type=info['model_type'],
            training_date=info['training_date'],
            n_features=info['n_features'],
            feature_names=info['feature_names'][:20],  # Return first 20 features
            model_loaded=True
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/analytics/fraud-metrics", tags=["Analytics"])
async def get_fraud_metrics(
    start_date: Optional[str] = Query(None, description="Start date (YYYY-MM-DD)"),
    end_date: Optional[str] = Query(None, description="End date (YYYY-MM-DD)")
):
    """Get fraud detection metrics for the dataset."""
    try:
        # Load data
        df = pd.read_csv('data/synthetic-claims.csv', comment='#')
        df['service_date'] = pd.to_datetime(df['service_date'])
        
        # Filter by date range if provided
        if start_date:
            df = df[df['service_date'] >= start_date]
        if end_date:
            df = df[df['service_date'] <= end_date]
        
        # Calculate metrics
        metrics = calculate_fraud_metrics(df)
        
        return metrics
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/analytics/high-risk-providers", response_model=List[ProviderRiskScore], tags=["Analytics"])
async def get_high_risk_providers(
    threshold: float = Query(0.05, ge=0, le=1, description="Fraud rate threshold"),
    limit: int = Query(10, ge=1, le=100, description="Maximum number of providers to return")
):
    """Identify providers with high fraud rates."""
    try:
        # Load data
        df = pd.read_csv('data/synthetic-claims.csv', comment='#')
        
        # Identify high-risk providers
        high_risk = identify_high_risk_providers(df, threshold)
        
        # Calculate risk scores
        high_risk['risk_score'] = (
            high_risk['fraud_rate'] * 0.5 +
            (high_risk['std_billed'] / (high_risk['avg_billed'] + 1)).fillna(0) * 0.3 +
            (high_risk['fraud_claims'] / high_risk['total_claims']) * 0.2
        )
        
        # Determine risk levels
        high_risk['risk_level'] = pd.cut(
            high_risk['risk_score'],
            bins=[0, 0.2, 0.5, 0.8, 1.0],
            labels=['Low', 'Medium', 'High', 'Critical']
        )
        
        # Convert to response format
        providers = []
        for provider_id, row in high_risk.head(limit).iterrows():
            providers.append(
                ProviderRiskScore(
                    provider_id=provider_id,
                    total_claims=int(row['total_claims']),
                    fraud_claims=int(row['fraud_claims']),
                    fraud_rate=float(row['fraud_rate']),
                    avg_billed=float(row['avg_billed']),
                    risk_score=float(row['risk_score']),
                    risk_level=str(row['risk_level'])
                )
            )
        
        return providers
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/analytics/fraud-types", tags=["Analytics"])
async def get_fraud_type_distribution():
    """Get distribution of fraud types in the dataset."""
    try:
        # Load data
        df = pd.read_csv('data/synthetic-claims.csv', comment='#')
        
        # Get fraud type distribution
        fraud_types = df[df['is_fraud'] == True]['fraud_type'].value_counts()
        
        return {
            "total_fraud_claims": int(df['is_fraud'].sum()),
            "fraud_types": fraud_types.to_dict()
        }
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

# Run the API
if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)