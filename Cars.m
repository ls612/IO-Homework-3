clear all

%Import the data into a table
cardata = readtable("C:\Users\Leo\OneDrive\School Stuff\IO\HW3\IO-Homework-3\cardata.txt");

%QUESTION 2: Maximum Likelihood estimation with exogenous price; need to review the
%likelihood function with only share data TODO
x_0 = [0 0 0 0 0];
%Need a ton of trys for the function to work
options = optimset('MaxFunEvals', 5000);
%This is actually minimizing the negative of the likelihood function, thus
%maximizing the likelihood function
maximumlikelihood = fminsearch(@likelihoodfn, x_0, options)

%QUESTION 3: Using the s_j - s_0 trick we can just do a simple linear
%regression, again with the assumption of exogenous price
alpha_hat_Q3 = zeros(20,1);
beta_hat_Q3 = zeros(20,4);
s_0 = zeros(20,1);
for i = 71:90
    %Separate out data by year for calculations and create a matrix of the
    %variables, and separate out x, s, p;
    cardatathisyear = cardata(cardata.year == i, :);
    cardatathisyearcharacteristics = removevars(cardatathisyear, {'vehicle_name','year','firmid'});
    
    %the data table is useful for going back to look at which car is which
    %so we keep it, but we also need the numerical data as a matrix for our
    %calculations
    carmatrix = table2array(cardatathisyearcharacteristics);
    p = carmatrix(:,5);
    s = carmatrix(:,6);
    X = carmatrix(:,1:4);
    
    %shares are already in log form so we need to exponentiate to get the
    %s_0
    s_0(i - 70) = 1 - sum(s);
    
    %ln(s_j) - ln(s_0) = X_j \beta + p_j \alpha based on the notes
    y = log(s) - log(s_0(i - 70));
    
    if i ~= 71
        %We remove the AC regressor and set it's coefficient equal to 0 in 1971
        %because no cars had it standard and keeping it in leads to rank
        %deficiency
        coefficients = regress(y, [X p]);
    else
        X_Funny = [X(:,1:2) X(:,4)];
        coefficients = regress(y, [X_Funny p]);
        Arr2=[0];
        coefficients=[coefficients(1:2); Arr2; coefficients(3:end)];
    end
    
    %Store the answers for the elasticity calculations
    alpha_hat_Q3(i - 70) = coefficients(5);
    beta_hat_Q3(i - 70,:) = coefficients(1:4);
end

%By algebra from our regession equation we get
%s_j = s_0 * exp(X_j \beta + p_j \alpha) + s_0
%elasticity is d(s_j)/d(p_j) * (p_j / s_j)
% = \alpha exp(X_j \beta + p_j \alpha) * (p_j / s_j)

%1990 Honda Accord Own Price Elasticity
HondaData = cardata((cardata.year == 90) & strcmp('HDACCO', cardata.vehicle_name), :);
HondaData = removevars(HondaData, {'vehicle_name','year','firmid'});
HondaData = table2array(HondaData);

honda_xb = dot(HondaData(1:4), beta_hat_Q3(20, :))
honda_ap = alpha_hat_Q3(20) * HondaData(5)
honda_accord_own_price_elasticity = alpha_hat_Q3(20) * s_0(20) * exp(honda_xb + honda_ap) * (HondaData(5)/HondaData(6))
