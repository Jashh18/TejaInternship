from fastapi import FastAPI
from pydantic import BaseModel
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import LabelEncoder, StandardScaler
from sklearn.model_selection import train_test_split

app = FastAPI()

# Load and preprocess the Titanic dataset
df = pd.read_csv("C:/Users/tunte/Desktop/Titanic-Dataset.csv")

# Preprocessing steps
df['Age'] = df['Age'].fillna(df['Age'].median())
df['Fare'] = df['Fare'].fillna(df['Fare'].median())

# Encode categorical variables
le = LabelEncoder()
df['Sex_encoded'] = le.fit_transform(df['Sex'])
df = pd.get_dummies(df, columns=['Embarked'], drop_first=False)

# Select features
selected_columns = ['Pclass', 'Sex_encoded', 'Age', 'SibSp', 'Parch', 'Fare'] + list(
    df.columns[df.columns.str.startswith('Embarked_')]
)
X = df[selected_columns]
y = df['Survived']

# Split and scale the data
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)
scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train)

# Train the Logistic Regression model
model = LogisticRegression(max_iter=1000, class_weight="balanced")
model.fit(X_train_scaled, y_train)

# Pydantic model for input validation
class PassengerData(BaseModel):
    Pclass: int
    Sex: str
    Age: int
    SibSp: int
    Parch: int
    Fare: float

@app.post("/predict")
def predict_survival(data: PassengerData):
    # Preprocess input data
    sex_encoded = le.transform([data.Sex])[0]
    input_data = [
        [
            data.Pclass,
            sex_encoded,
            data.Age,
            data.SibSp,
            data.Parch,
            data.Fare
        ]
    ]
    
    # Add 'Embarked' columns with zero as default for input (assumes no specific 'Embarked' data)
    for col in df.columns[df.columns.str.startswith('Embarked_')]:
        input_data[0].append(0)

    # Scale input data
    input_data_scaled = scaler.transform(input_data)

    # Make prediction
    prediction = model.predict(input_data_scaled)[0]
    survival_status = "Survived" if prediction == 1 else "Did not survive"
    return {"Prediction": survival_status}