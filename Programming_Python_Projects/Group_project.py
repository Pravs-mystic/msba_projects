import numpy as np
import pandas as pd

# Define a function to forecast airline demand using different models
def airlineForecast(trainingDataFileName,validationDataFileName):
    # Load training and validation data from CSV files
    dfTraining = pd.read_csv(trainingDataFileName)
    dfValidation = pd.read_csv(validationDataFileName)

    # Preprocess the data by calculating days prior to departure and final demand
    daysPrior(dfTraining)
    daysPrior(dfValidation)
    dfTraining = finalDemand(dfTraining)
    dfValidation = finalDemand(dfValidation)  
    
    # Apply different forecasting models: additive, multiplicative, and additive with day of week
    additive_model = additive(dfTraining,dfValidation)
    multiplicative_model = multiplicative(dfTraining,dfValidation)
    additive_DOW_model = additive_DOW(dfTraining,dfValidation)
   
    # Calculate and compare the MASE (Mean Absolute Scaled Error) for each model to find the best one
    MASE_list = [additive_model[0], multiplicative_model[0], additive_DOW_model[0]]
    best_mase = min(MASE_list)
   
    # Print and return the best model based on MASE value
    print( "\n ***The MASE value and Dataframe of the best model is as follows***  \n" )
   
    if best_mase == additive_model[0]:
        print("Additive_model \n")
        return additive_model
    elif best_mase == multiplicative_model[0]:
        print("Multiplicative_model \n")
        return multiplicative_model
    else:
        print("Additive_DOW_model \n")
        return additive_DOW_model

# Define a function to implement the additive model for forecasting
def additive(dfTraining, dfValidation):
    # Calculate the remaining demand and average remaining demand for training data
    dfTraining['remainingDemand'] = dfTraining['finalDemand'] - dfTraining['cum_bookings']
    subset_df=pd.DataFrame()
    subset_df['averageremdemand'] =dfTraining.groupby(['daysPrior']).mean(numeric_only=True)['remainingDemand']

    # Merge with validation data and calculate forecast values
    mergedDf = pd.merge(dfValidation, subset_df, on='daysPrior', how='left')
    mergedDf['forecast'] = mergedDf['averageremdemand'] + mergedDf['cum_bookings']
    finalDf = mergedDf.dropna()
    forecast_df = finalDf.loc[:,['departure_date', 'booking_date', 'forecast']]

    # Calculate and return the MASE value along with the forecast dataframe
    MASE_additive = calculateMASE(finalDf)
    return [MASE_additive, forecast_df]
   
# Define a function to implement the multiplicative model for forecasting
def multiplicative(dfTraining, dfValidation):
    # Calculate the average remaining demand for training data in a multiplicative manner
    subset_df_multi = pd.DataFrame()
    subset_df_multi['averageremdemand'] = (dfTraining.groupby(['daysPrior']).mean(numeric_only=True)['cum_bookings'])/(dfTraining.groupby(['daysPrior']).mean(numeric_only=True)['finalDemand'])

    # Merge with validation data and calculate forecast values
    mergedDf_multi = pd.merge(dfValidation, subset_df_multi, on='daysPrior', how='left')
    mergedDf_multi['forecast'] = mergedDf_multi['cum_bookings']/mergedDf_multi['averageremdemand']
    finalDfMulti = mergedDf_multi.dropna()
    forecast_df = finalDfMulti.loc[:,['departure_date', 'booking_date', 'forecast']]

    # Calculate and return the MASE value along with the forecast dataframe
    MASE_multiplicative = calculateMASE(finalDfMulti)
    return [MASE_multiplicative, forecast_df]
       
# Define a function to implement the additive model with day of the week for forecasting
def additive_DOW(dfTraining, dfValidation):    #Additive + DOW
    # Calculate the average remaining demand based on days prior and booking day of week
    subset_df_dow = pd.DataFrame()
    subset_df_dow['averageremdemand'] = dfTraining.groupby(['daysPrior', 'booking_day_of_week']).mean(numeric_only=True)['remainingDemand']
    merged_df_dow = pd.merge(dfValidation, subset_df_dow, on=['daysPrior', 'booking_day_of_week'], how='left')
    merged_df_dow['forecast'] = merged_df_dow['averageremdemand'] + merged_df_dow['cum_bookings']
    finalDfDow = merged_df_dow.dropna()
    forecast_df = finalDfDow.loc[:,['departure_date', 'booking_date', 'forecast']]

    # Calculate and return the MASE value along with the forecast dataframe
    MASE_additive_DOW = calculateMASE(finalDfDow)
    return [MASE_additive_DOW, forecast_df]

# Define a function to calculate Mean Absolute Scaled Error (MASE) for additive model
def calculateMASE(masedf):
    '''
    Calculate Mean Absolute Scaled Error (MASE) for additive model
    '''
    # Calculate absolute errors for naive forecasts and model forecasts
    naive_forecasts_absolute_error = (masedf['finalDemand'] - masedf['naive_fcst']).abs().sum()
    model_forecasts_absolute_error = (masedf['finalDemand'] - masedf['forecast']).abs().sum()

    # Calculate and return MASE value
    MASE = model_forecasts_absolute_error / naive_forecasts_absolute_error
    return (round(MASE,3))

# Define a function to calculate the final demand for each flight
def finalDemand(df):
    # Extract final demand (cumulative bookings on departure date) for each flight
    frame = df.loc[df.departure_date == df.booking_date, ['departure_date','cum_bookings']]
    df = pd.merge(df, frame, on='departure_date', how='left')
    df = df.rename(columns={'cum_bookings_y': 'finalDemand'})
    df = df.rename(columns={'cum_bookings_x': 'cum_bookings'})
    return df

# Define a function to calculate the number of days prior to departure and the booking day of the week
def daysPrior(df):
    # Convert date strings to datetime objects and calculate days prior to departure
    df['departure_date'] = pd.to_datetime(df['departure_date'])
    df['booking_date'] = pd.to_datetime(df['booking_date'])
    df['daysPrior'] = (df['departure_date'] - df['booking_date']).dt.days
    df['booking_day_of_week'] = df['booking_date'].dt.day_name()
    return df

# Main function to execute the program
def main():
    # Call the airlineForecast function with training and validation data files and print the result
    print(airlineForecast("airline_data_training.csv", "airline_data_validation.csv"))

# Execute the main function
main()
