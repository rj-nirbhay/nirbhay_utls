from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = "all"

import pandas as pd
import numpy as np

import subprocess
import sys

def install(package):
    """
    This function install the packages using pip package 
        params:
            package- package name in string 
        output:
            connectionString : user connect details
    """
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

install('sinfo') # to get session info
from sinfo import sinfo
sinfo()

def yearMonSeqGenerator(startDate='1970-01-01',endDate=pd.to_datetime('today')):
    """
    This function creats the sequence of month and year
    params:
        startDate- period start date
        endDate- period end date
    output:
        df : date seq
    demo:
        smry=yearMonSeqGenerator('1970-01-01','2020-01-01')
    """
    dtRng=pd.date_range(startDate,endDate,freq='MS')
    return pd.DataFrame(
        {"YearMonStr":dtRng.strftime("%Y%b").tolist(),
         "YearMonNum":dtRng.strftime("%Y%m").tolist()})

def GetUnivariateSmry(ds,quantileCuts=[0.05 , 0.1, 0.2, 0.25,0.3, 0.4, 0.5, 0.6, 0.7, 0.75,0.8, 0.9, 0.95,0.98,0.99]):
    """
    This function calculate the basic univariate functions
    """
#     Quantile distn:
    d1 = ds.quantile(quantileCuts).T
    d1.reset_index(inplace=True)
    qNames = [f'Q{int(x* 100)}' for x in quantileCuts]
    newNames = ['index']
    newNames.extend(qNames)
    d1.columns = newNames    
    
#     Other Basic metrics
    d2 = pd.DataFrame(ds.isna().sum(),columns = ['NullCount'])
    d2['DataType'] = d2.index.map(ds.dtypes)
    d2['BlankCount'] = d2.index.map((ds=='').sum())
    d2['NonNullCount'] = d2.index.map(ds.notna().sum())
    d2['FillPerc']= round(d2['NonNullCount']/ds.shape[0],2)
    d2['UniqueCount'] = d2.index.map(ds.nunique())
    d2['Min'] = ds.min(numeric_only=True)
    d2['Mean'] = ds.mean()
    d2['NonZeroMean'] = ds.replace(0, np.nan).mean()
    d2['Max'] = ds.max(numeric_only=True)
    d2['Total']= ds.sum(numeric_only=True)
    d2['std'] = ds.std()
    d2['skewness'] = ds.skew()
    d2['kurtosis'] = ds.kurtosis()
    d2.reset_index(inplace=True)
    
#     creating master summary
    d = d2.merge(d1, on='index', how='left')
    d.rename(columns={"index":"ParameterName"},inplace=True)
    
#     re-arranging columns
    first_cols = ['ParameterName','DataType']
    last_cols = [col for col in d.columns if col not in first_cols]
    d = d[first_cols+last_cols]
    
    return d


def GetFreq(ds,colList=None,cutoff=100):
    """
    This function creates freq distn based on user input for number of levels or vars
    param:
        ds: dataframe
        colList : column names list for which freq to be fetched
        cutoff : max number of levels for variables, variable with more than cutoff will be dropped for the freq calculation
    
    """
    if colList is None:
        colList=ds.nunique()
        colList=colList[(colList<=cutoff) & (colList>0)].index.tolist()
    else :
        colList=colList
    
    #d = pd.DataFrame((ds[colList].apply(pd.value_counts).T.stack())) # it doesn't gives nan values
    d = pd.DataFrame(ds[colList].apply(lambda x: x.value_counts(dropna=False)).T.stack())
    d.reset_index(inplace=True)
    d.rename(columns={"level_0":"FeatureName","level_1":"Levels",0:"Freq"},inplace=True)
    d['Levels'] = d['Levels'].fillna("NA") #replace nan to NA
    d['Prop'] = d['Freq']/ds.shape[0]
    
    return d
