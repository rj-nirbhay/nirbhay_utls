from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = "all"

import pandas as pd
import numpy as np

import subprocess
import sys

# -----------------------------------------------------------------------------------------------------------------
# Function 1: Install packages and get requirement txt
# -----------------------------------------------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------------------------------------------
# Function 2: Year month sequence genearator
# -----------------------------------------------------------------------------------------------------------------


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

# -----------------------------------------------------------------------------------------------------------------
# Function 3: Univariate summary of dataset
# -----------------------------------------------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------------------------------------------
# Function 4: Get freq of each level in a features
# -----------------------------------------------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------------------------------------------
# Function 5: Get Re-order columns
# -----------------------------------------------------------------------------------------------------------------

def reorder_columns(df,first_cols=['']):
    """
    This function reorder columns in a dataset
    param:
        df:dataframe
        first_cols: list of columns which should be at left
    """

    last_cols = [col for col in df.columns if col not in first_cols]
    df = df[first_cols+last_cols]
    return(df)

# -----------------------------------------------------------------------------------------------------------------
# Function 6: correlated variables pair, VIF
# -----------------------------------------------------------------------------------------------------------------

def correl_vars(ds,cutoff=0.65, is_cor_mat_return=True):
    """
    This functions gives pair var list which are correlated based on the cutoff 
    param:
        ds : dataframe
        cutoff : cutoff to choose correl level
        is_cor_mat_return : True if correlation matrix to be return
    """
    cor_mat = ds.corr() # correl matrix
    
    var1 = []; var2 = []
    for i in range(len(cor_mat.columns)):
        for j in range(len(cor_mat.index)):
            if (abs(cor_mat.iloc[i,j]) > cutoff) & (i>j):
                var1.append(cor_mat.columns[i]); var2.append(cor_mat.index[j])
    
    high_cor_var = list(zip(var1,var2)) # correls vars list
    
    # Getting VIF's
    inv_corr_mat = np.linalg.inv(corr_mat)
    vif = pd.DataFrame(np.diag(inv_corr_mat), index=df.columns).reset_index().rename(columns={'index':'Parameter',0:'VIF'}).sort_values(by = ['VIF'],ascending=False, ignore_index=True)
    
    # Other way by using statsmodels package : added intercept using add_constant as statmodels doesn't include it by default
#     from statsmodels.stats.outliers_influence import variance_inflation_factor
#     from statsmodels.tools.tools import add_constant
#     vif = pd.DataFrame([variance_inflation_factor(add_constant(ds).values, i) for i in range(add_constant(ds).shape[1])], \
#                          index=add_constant(ds).columns, columns=['VIF']).reset_index().rename(columns={'index':'Parameter'}).drop(index=0).sort_values(by = ['VIF'],ascending=False, ignore_index=True)
    
    if is_cor_mat_return :
        correl_dict = {'correl_matrix':cor_mat, 'Correl_vars' : high_cor_var, 'vif':vif}
        return correl_dict
    else :
        correl_dict = {'Correl_vars' : high_cor_var, 'vif':vif}
        return correl_dict
   
# -----------------------------------------------------------------------------------------------------------------
# Function 7: Concat multiple columns based on seperator
# -----------------------------------------------------------------------------------------------------------------

from functools import reduce
def reduce_join(df, columns,sep='_'):
    """
    Concat multiple string columns
    param:
        df : dataframe
        columns : list of col names
        sep : seperator
        
    """
    assert len(columns) > 1
    slist = [df[x].astype(str) for x in columns]
    return reduce(lambda x, y: x + sep + y, slist[1:], slist[0])

# -----------------------------------------------------------------------------------------------------------------
# Function 8: Correlation Matrix and heatmap for all features in dataset 
# -----------------------------------------------------------------------------------------------------------------
# Owner : Not Nirbhay
def convert(data, to):
    converted = None
    if to == 'array':
        if isinstance(data, np.ndarray):
            converted = data
        elif isinstance(data, pd.Series):
            converted = data.values
        elif isinstance(data, list):
            converted = np.array(data)
        elif isinstance(data, pd.DataFrame):
            converted = data.as_matrix()
    elif to == 'list':
        if isinstance(data, list):
            converted = data
        elif isinstance(data, pd.Series):
            converted = data.values.tolist()
        elif isinstance(data, np.ndarray):
            converted = data.tolist()
    elif to == 'dataframe':
        if isinstance(data, pd.DataFrame):
            converted = data
        elif isinstance(data, np.ndarray):
            converted = pd.DataFrame(data)
    else:
        raise ValueError("Unknown data conversion: {}".format(to))
    if converted is None:
        raise TypeError('cannot handle data conversion of type: {} to {}'.format(type(data),to))
    else:
        return converted
    
def conditional_entropy(x, y):
    """
    Calculates the conditional entropy of x given y: S(x|y)
    Wikipedia: https://en.wikipedia.org/wiki/Conditional_entropy
    :param x: list / NumPy ndarray / Pandas Series
        A sequence of measurements
    :param y: list / NumPy ndarray / Pandas Series
        A sequence of measurements
    :return: float
    """
    # entropy of x given y
    y_counter = Counter(y)
    xy_counter = Counter(list(zip(x,y)))
    total_occurrences = sum(y_counter.values())
    entropy = 0.0
    for xy in xy_counter.keys():
        p_xy = xy_counter[xy] / total_occurrences
        p_y = y_counter[xy[1]] / total_occurrences
        entropy += p_xy * math.log(p_y/p_xy)
    return entropy

def cramers_v(x, y):
    confusion_matrix = pd.crosstab(x,y)
    chi2 = ss.chi2_contingency(confusion_matrix)[0]
    n = confusion_matrix.sum().sum()
    phi2 = chi2/n
    r,k = confusion_matrix.shape
    phi2corr = max(0, phi2-((k-1)*(r-1))/(n-1))
    rcorr = r-((r-1)**2)/(n-1)
    kcorr = k-((k-1)**2)/(n-1)
    return np.sqrt(phi2corr/min((kcorr-1),(rcorr-1)))

def theils_u(x, y):
    s_xy = conditional_entropy(x,y)
    x_counter = Counter(x)
    total_occurrences = sum(x_counter.values())
    p_x = list(map(lambda n: n/total_occurrences, x_counter.values()))
    s_x = ss.entropy(p_x)
    if s_x == 0:
        return 1
    else:
        return (s_x - s_xy) / s_x

def correlation_ratio(categories, measurements):
    fcat, _ = pd.factorize(categories)
    cat_num = np.max(fcat)+1
    y_avg_array = np.zeros(cat_num)
    n_array = np.zeros(cat_num)
    for i in range(0,cat_num):
        cat_measures = measurements[np.argwhere(fcat == i).flatten()]
        n_array[i] = len(cat_measures)
        y_avg_array[i] = np.average(cat_measures)
    y_total_avg = np.sum(np.multiply(y_avg_array,n_array))/np.sum(n_array)
    numerator = np.sum(np.multiply(n_array,np.power(np.subtract(y_avg_array,y_total_avg),2)))
    denominator = np.sum(np.power(np.subtract(measurements,y_total_avg),2))
    if numerator == 0:
        eta = 0.0
    else:
        eta = numerator/denominator
    return eta

def associations(dataset, nominal_columns=None, mark_columns=False, theil_u=False, plot=True,
                          return_results = False, **kwargs):
    """
    Calculate the correlation/strength-of-association of features in data-set with both categorical (eda_tools) and
    continuous features using:
     - Pearson's R for continuous-continuous cases
     - Correlation Ratio for categorical-continuous cases
     - Cramer's V or Theil's U for categorical-categorical cases
    :param dataset: NumPy ndarray / Pandas DataFrame
        The data-set for which the features' correlation is computed
    :param nominal_columns: string / list / NumPy ndarray
        Names of columns of the data-set which hold categorical values. Can also be the string 'all' to state that all
        columns are categorical, or None (default) to state none are categorical
    :param mark_columns: Boolean (default: False)
        if True, output's columns' names will have a suffix of '(nom)' or '(con)' based on there type (eda_tools or
        continuous), as provided by nominal_columns
    :param theil_u: Boolean (default: False)
        In the case of categorical-categorical feaures, use Theil's U instead of Cramer's V
    :param plot: Boolean (default: True)
        If True, plot a heat-map of the correlation matrix
    :param return_results: Boolean (default: False)
        If True, the function will return a Pandas DataFrame of the computed associations
    :param kwargs:
        Arguments to be passed to used function and methods
    :return: Pandas DataFrame
        A DataFrame of the correlation/strength-of-association between all features
    :E.g. 
    results = associations(df,nominal_columns='all',return_results=True)
    """

    dataset = convert(dataset, 'dataframe')
    columns = dataset.columns
    if nominal_columns is None:
        nominal_columns = list()
    elif nominal_columns == 'all':
        nominal_columns = columns
    corr = pd.DataFrame(index=columns, columns=columns)
    for i in range(0,len(columns)):
        for j in range(i,len(columns)):
            if i == j:
                corr[columns[i]][columns[j]] = 1.0
            else:
                if columns[i] in nominal_columns:
                    if columns[j] in nominal_columns:
                        if theil_u:
                            corr[columns[j]][columns[i]] = theils_u(dataset[columns[i]],dataset[columns[j]])
                            corr[columns[i]][columns[j]] = theils_u(dataset[columns[j]],dataset[columns[i]])
                        else:
                            cell = cramers_v(dataset[columns[i]],dataset[columns[j]])
                            corr[columns[i]][columns[j]] = cell
                            corr[columns[j]][columns[i]] = cell
                    else:
                        cell = correlation_ratio(dataset[columns[i]], dataset[columns[j]])
                        corr[columns[i]][columns[j]] = cell
                        corr[columns[j]][columns[i]] = cell
                else:
                    if columns[j] in nominal_columns:
                        cell = correlation_ratio(dataset[columns[j]], dataset[columns[i]])
                        corr[columns[i]][columns[j]] = cell
                        corr[columns[j]][columns[i]] = cell
                    else:
                        cell, _ = ss.pearsonr(dataset[columns[i]], dataset[columns[j]])
                        corr[columns[i]][columns[j]] = cell
                        corr[columns[j]][columns[i]] = cell
    corr.fillna(value=np.nan, inplace=True)
    if mark_columns:
        marked_columns = ['{} (nom)'.format(col) if col in nominal_columns else '{} (con)'.format(col) for col in columns]
        corr.columns = marked_columns
        corr.index = marked_columns
    if plot:
        plt.figure(figsize=(20,20))#kwargs.get('figsize',None))
        sns.heatmap(corr, annot=kwargs.get('annot',True), fmt=kwargs.get('fmt','.2f'), cmap='coolwarm')
        plt.show()
    if return_results:
        return corr
    
# -----------------------------------------------------------------------------------------------------------------
# Function 9: Correlation Matrix for categorical features in dataset 
# -----------------------------------------------------------------------------------------------------------------

def cramers_v1(confusion_matrix):
    """ calculate Cramers V statistic for categorial-categorial association.
        uses correction from Bergsma and Wicher,
        Journal of the Korean Statistical Society 42 (2013): 323-328
    """
    chi2 = ss.chi2_contingency(confusion_matrix)[0]
    n = confusion_matrix.sum()
    phi2 = chi2 / n
    r, k = confusion_matrix.shape
    phi2corr = max(0, phi2 - ((k-1)*(r-1))/(n-1))
    rcorr = r - ((r-1)**2)/(n-1)
    kcorr = k - ((k-1)**2)/(n-1)
    return np.sqrt(phi2corr / min((kcorr-1), (rcorr-1)))


#Generate correlation matrix using cramers_v1 function
def cat_correl_matrix(df, col_list = None):
    """ calculate correlation matrix using Cramers V statistic based on cramers_v1 for categorial-categorial association.
    args:
        df - dataframe
        col_list - varibale list for which correlation to be calculated
    E.g. 
        cat_correl_matrix(data,['var1','var2'])
                        or
        cat_correl_matrix(data)

    """

    if (col_list == None ):
    #select features for which correlations needs to be calculated
     cat_col = df.select_dtypes(['category']).columns

    else : 
        cat_col = col_list

    if (len(cat_col) == 0) : 
        return (print('* Categoical columns are not present in input dataset.'+ str('\n')+ 
                      '* Please change datatypes to categorical for required features'))
    else :

        correl_mat =pd.DataFrame(data='',index=cat_col,columns=cat_col)
        #calculating correlation matrix
        for i in range(len(cat_col)):
            for j in range(i):
                confusion_matrix = pd.crosstab(df[cat_col[i]], df[cat_col[j]]).as_matrix()
                correl_mat.iloc[i,j]= round(100*cramers_v1(confusion_matrix),2)
        #Output 
        print("Correlation Matrix of categorical variables are:-")
        return correl_mat
    
# -----------------------------------------------------------------------------------------------------------------
# Function 10: Basic Distribution Bar Grpah
# -----------------------------------------------------------------------------------------------------------------

def barGraph(df,path):
    for column in df:
        """
        Creats bar graphs of all columns in a dataset
        args:
            df - dataFrame
            path- location where graphs will be saved
        """

        # create a figure and axis 
        _=fig, ax = plt.subplots(figsize=(8, 4));
        _=data1 = df[column].value_counts()/len(df)
        _=ax.bar(data1.index, data1.values,width=0.4, color = 'blue') 
        _=ax.set_title(str(column), y=0.9)
        _=ax.set_ylim([0,1])
        _=ax.set_ylabel('Distribution %')
        _=ax.set_xticks(np.arange(len(data1.index)))


        for rect in ax.bar(data1.index, data1.values,width=0.4):
            _=height =rect.get_height()
            _=ax.text(rect.get_x() + rect.get_width()/2.0, height, f'{np.round(100*height,1)}%' , \
                      ha='center', va='bottom',fontsize=10)
            #fontweight='bold'
        plt.savefig(f'{path}\\{column}.png', dpi=300, format='png', bbox_inches='tight')
   
# -----------------------------------------------------------------------------------------------------------------
# Function 11: Paste equivalent of R
# -----------------------------------------------------------------------------------------------------------------

def paste(List, sep=''):
    """
    Creates a str object
    args:
        List - list or range 
        sep- seperator
    e.g: 
        Input- l=['a','b','c','d','e']
        Syntax- paste(l)
        Output- 'abcde'
        
    """
    strCombn =str()
    for i in range(len(List)):
        temp= f'{List[i]}'
        if (i==0):
            strCombn= temp
        else:
            strCombn = f'{strCombn}{sep}{temp}'
        
    return strCombn

# -----------------------------------------------------------------------------------------------------------------
# Function 12: Summary of groupby object
# -----------------------------------------------------------------------------------------------------------------

def getGrpStats(group):
    """
    function that extract statistical parameters from a grouby objet
    args:
        group variables
    syntax:
        global_stats = df['AMOUNT'].groupby(df['ORG_NAME']).apply(get_stats).unstack()
    """
    return {'min': group.min(), 'max': group.max(),
            'count': group.count(), 'mean': group.mean(), 'sum':group.sum()}


# -----------------------------------------------------------------------------------------------------------------
# Function 13: Generate Date for a period
# -----------------------------------------------------------------------------------------------------------------


def dateGenerator (startDate,endDate,freq='D',missingDays=None):
    """
    args: 
        startDate- period Start Date
        endDate- period end date
        freq- Interval at which dates to be generated, default 'D' for daily
        missingDays - List of day which user wants to omit
    eg:
        dateGenerator('1-1-2017','12-31-2018',freq='3M',missingDays=['Saturday'])
                                        or
        dateGenerator('1-1-2017','12-31-2018',freq='3m',missingDays=['Saturday'])
        dateGenerator('1-1-2017','12-31-2018')
    Output:
                0
        0	2017-01-02
        1	2017-01-03
    """
    date = pd.date_range(start=startDate, end=endDate, freq=freq)
    
    if (missingDays is None):
        return(date) 
    else : 
        return(date[~date.strftime('%A').isin(missingDays)])

# -----------------------------------------------------------------------------------------------------------------
# Function 14: Change integer into ordinal format
# -----------------------------------------------------------------------------------------------------------------

import math
"""
This function convert integer into it's ordinal value
--dependencie import math
--args: integer number
Eg: 
    syntax- ordinal(10)
    output- '10th'
    or
    syntax- print([ordinal(n) for n in range(1,5)]) #for 
    output- ['1st', '2nd', '3rd', '4th']
"""
ordinal= lambda n: "%d%s" % (n,"tsnrhtdd"[(math.floor(n/10)%10!=1)*(n%10<4)*n%10::4])


# -----------------------------------------------------------------------------------------------------------------
# Function 15: Outlier tagging based on the std dev
# -----------------------------------------------------------------------------------------------------------------

def rem_xsd_more(data_col,th=3):
    # Function to tag observation as outlier based on predefined threshold in terms of standard deviations
    # args: 
        # data_col: data columns
        # th: standard deviation as threshold
    # returns: True if outlier , False if non-outlier 
    mean = data_col.mean()
    std = data_col.std()
    return((data_col-mean)/std>th)

# -----------------------------------------------------------------------------------------------------------------
# Function 16: Outlier tagging based on the quantile
# -----------------------------------------------------------------------------------------------------------------
def rem_xpctl_more(data_col,pctl_th=0.99):
    # Function to tag observation as outlier based on predefined threshold in terms of quantile
    # args: 
        # data_col: data columns
        # th: percentile as threshold
    # returns: True if outlier , False if non-outlier 
    pctl = data_col.quantile(pctl_th)
    return data_col>pctl

# -----------------------------------------------------------------------------------------------------------------
# Function 17: F-test
# -----------------------------------------------------------------------------------------------------------------
import numpy as np
from scipy import stats
import pandas as pd

#define F-test function
def f_test(x, y):
    x = np.array(x)
    y = np.array(y)
    f = np.var(x, ddof=1)/np.var(y, ddof=1) #calculate F test statistic 
    dfn = x.size-1 #define degrees of freedom numerator 
    dfd = y.size-1 #define degrees of freedom denominator 
    p = 1-scipy.stats.f.cdf(f, dfn, dfd) #find p-value of F test statistic 
    return f, p

#perform F-test
# f_test(df['var1'],df['var2'])

# -----------------------------------------------------------------------------------------------------------------
# Function 18: variance threshold selector
# -----------------------------------------------------------------------------------------------------------------
from sklearn.feature_selection import VarianceThreshold
def variance_threshold_selector(data, threshold=0.5):
    selector = VarianceThreshold(threshold)
    selector.fit(data)
    return data[data.columns[selector.get_support(indices=True)]]

# -----------------------------------------------------------------------------------------------------------------
# Function 19: Decile wise Event rate and population plot for logistic regression
# -----------------------------------------------------------------------------------------------------------------
def logit_model_plots(ds,Population = 'Population_%',Event_rate ='Event_rate',decile ='Band',Cumulative_Non_Event = 'Cumulative_Non_Event_%',Cumulative_Event= 'Cumulative_Event_%',sample_type ='Development'):
    """
    function that plot band wise event rate and population
    args:
        ds : dataset with population, band, event_rate
        Population : str, feature name for population
        Event_rate : str, feature name for Event_rate
        Cumulative_Non_Event : str, feature name for Cumulative_Non_Event
        Cumulative_Event : str, feature name for Cumulative_Event
        decile : str, feature name for decile/bands
        sample_type : str, feature name for sample_type , used in title of the plot
        
    returns: plots
    """
    
    import matplotlib.pyplot as plt
    fig, (ax1, ax2) = plt.subplots(1, 2,figsize=(15, 4))
    _= ax1.plot(plot_df[Cumulative_Non_Event],plot_df[Cumulative_Event])
    _= ax1.set_ylabel(Cumulative_Non_Event)
    _= ax1.set_title('Gini Curve : '+str(sample_type) +' sample')
    _= ax1.set_xlabel(Cumulative_Event)

    _= plot_df[Population].plot(kind='bar', color='b', width = 0.35,legend=True , label = Population)
    _= plot_df[Event_rate].plot(kind='line',color ='r', secondary_y=True,legend=True, label = Event_rate)
    _= ax2.set_xticklabels(plot_df[decile])
    _= ax2.set_ylim(0,plot_df[Event_rate].max()*0.15)
    _= ax2.right_ax.set_ylim(0,plot_df[Event_rate].max()*1.5)
    _= ax2.right_ax.set_ylabel(Event_rate)
    _= ax2.set_ylabel(Population)
    _= ax2.set_title('Decile Wise Event Rate : ' +str(sample_type) +' sample')
    _= ax2.set_xlabel(decile)
    plt.show()

# -----------------------------------------------------------------------------------------------------------------
# Function 20: Read and write data from google sheet
# -----------------------------------------------------------------------------------------------------------------

def gsheet_handler(spread_workbook:str, sheet_name:str,path_to_credentials:str('super_user.json'), method='Read',action = 'append_rows',is_add_sheet=False, df=None,row_cutoff=0,col_cutoff=0,keep_headers=False):
    """
    Read and write data from/to google sheet
    
    Parameters
    ----------
    df : pandas dataframe
        new dataframe which to be added
    spread_workbook : str
        worksheet name
    sheet_name : str
        sheet to be updated
    path_to_credentials : str
        credentail json file path.
    is_add_sheet : bool, optional
        DESCRIPTION. if sheet to addded.The default is False.
    method : str
        'Read' : read data
        'write': write data to google sheet
    action : str
        'refresh_sheet' : write post clearing the eisting data
        'append_rows' : append rows to existing data 
        'append_columns': add columns to existing data

    Returns
    -------
    None.

    """
    
    scopes = ['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive',
              'https://www.googleapis.com/auth/spreadsheets']
    
    credentials = ServiceAccountCredentials.from_json_keyfile_name(path_to_credentials, scopes=scopes)
    gsc = gspread.authorize(credentials)
    ws = gsc.open(spread_workbook)
    
    if is_add_sheet ==False:
        
        # Get existing sheet data
        wb = ws.worksheet(sheet_name)
        wb_df = wb.get_all_records()
        wb_df = pd.DataFrame.from_dict(wb_df)
        
        if wb_df is not None:
            n_row = wb_df.shape[0] 
            n_col = wb_df.shape[1]
    
    if method == 'Read':
        
        return wb_df
    
    elif (method =='write') & (is_add_sheet):
        wb = ws.add_worksheet(rows=10000,cols=100,title=sheet_name)
        gd.set_with_dataframe(wb,df,include_column_header= keep_headers)
    
    elif (method =='write') & (action == 'refresh_sheet'):
        wb.clear()
        gd.set_with_dataframe(wb,df,row=1+row_cutoff,include_column_header=keep_headers) 
        
    elif (method =='write') & (action == 'append_rows'):
        gd.set_with_dataframe(wb,df,row=n_row+1+row_cutoff,include_column_header=keep_headers) 
        
    elif (method =='write') & (action == 'append_columns'):
        gd.set_with_dataframe(wb,df,col=n_col+1+col_cutoff,include_column_header=keep_headers) 
    
    else:
        print("None action are performed")
        
    return wb

# -----------------------------------------------------------------------------------------------------------------
# Function 21: Duplicated records based on column
# -----------------------------------------------------------------------------------------------------------------

def Get_dup_records(ds,key_var):
    """
    This function returns duplicate records
    args:
        ds : dataframe
        key_var : str, Variable name based on which duplcation present
    """
    temp = ds.groupby([key_var]).agg({key_var:'count'}).rename(columns={key_var:'Freq'}).reset_index()
    temp = temp[temp['Freq']>1]
    print("Total Duplicate records:: " +str(temp.shape[0]))

    return temp

# -----------------------------------------------------------------------------------------------------------------
# Function 22: Find all numberic number in string
# -----------------------------------------------------------------------------------------------------------------
import re
def find_number(text):
    num = re.findall(r'[0-9]+',text)
    return " ".join(num)

# -----------------------------------------------------------------------------------------------------------------
# Function 23: Multiple sheet output in excel
# -----------------------------------------------------------------------------------------------------------------

def dfs_tabs(df_list, sheet_list, file_name):
    """
    This function export data to multiple sheets of excel
    param:
        df_list : list of dataframes
        sheet_list : list of sheet name where data to exported
        file_name : Export file name 
        
    E.g :
        # list of dataframes and sheet names
        #dfs = [Req_tracker_asofdate, Req_tracker_agg]
        #sheets = ['Flow_Trends','Flow_Aggregate_Data']    
        #dfs_tabs(dfs, sheets, f"test.xlsx")
    
    """

    writer = pd.ExcelWriter(file_name,engine='xlsxwriter')   
    for dataframe, sheet in zip(df_list, sheet_list):
        dataframe.to_excel(writer, sheet_name=sheet, startrow=0 , startcol=0, index=False)   
    writer.save()
   
# -----------------------------------------------------------------------------------------------------------------
# Function 24: column names to proper format
# -----------------------------------------------------------------------------------------------------------------

def standardize_col_names(ds):
    """
    
    """
    ds.columns = ds.columns.str.lower()
    ds.columns = [re.sub(r"[^\w\s]", '_', col) for col in ds.columns ]
    ds.columns = ds.columns.str.replace('^ +| +$', '_')
    ds.columns = ds.columns.str.replace('__', '_')

    return ds

# -----------------------------------------------------------------------------------------------------------------
# Function 25: Bayesian Optimization for random forest
# -----------------------------------------------------------------------------------------------------------------
def bayesian_optimization(dataset, function, parameters):
    X_train, y_train, X_test, y_test = dataset
    n_iterations = 5
    gp_params = {"alpha": 1e-4}

    BO = BayesianOptimization(function, parameters)
    BO.maximize(n_iter=n_iterations, **gp_params)

    return BO.max

def rfc_optimization(cv_splits):
    def function(n_estimators, max_depth, min_samples_split):
        return cross_val_score(
               RandomForestClassifier(
                   n_estimators=int(max(n_estimators,0)),                                                               
                   max_depth=int(max(max_depth,1)),
                   min_samples_split=int(max(min_samples_split,2)), 
                   n_jobs=-1, 
                   random_state=42,   
                   class_weight="balanced"),  
               X=X_train, 
               y=y_train, 
               cv=cv_splits,
               scoring="roc_auc",
               n_jobs=-1).mean()

    parameters = {"n_estimators": (200, 300),
                  "max_depth": (8, 20),
                  "min_samples_split": (2, 10)}
    
    return function, parameters

def train(X_train, y_train, X_test, y_test, function, parameters):
    dataset = (X_train, y_train, X_test, y_test)
    cv_splits = 4
    
    best_solution = bayesian_optimization(dataset, function, parameters)      
    params = best_solution["params"]

    model = RandomForestClassifier(
             n_estimators=int(max(params["n_estimators"], 0)),
             max_depth=int(max(params["max_depth"], 1)),
             min_samples_split=int(max(params["min_samples_split"], 2)), 
             n_jobs=-1, 
             random_state=42,   
             class_weight="balanced")

    model.fit(X_train, y_train)
    
    return model

# -----------------------------------------------------------------------------------------------------------------
# Function 26: 
# -----------------------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------------------
# Function 27: 
# -----------------------------------------------------------------------------------------------------------------
    
