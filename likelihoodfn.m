function L = likelihoodfn(c)
    L = 1;
    cardata = readtable("C:\Users\Leo\OneDrive\School Stuff\IO\HW3\IO-Homework-3\cardata.txt");
    
    for year = 71:90
        %Separate out data by year for calculations and create a matrix of the
        %variables, and separate out x, s, p;
        cardatathisyear = cardata(cardata.year == year, :);
        cardatathisyearcharacteristics = removevars(cardatathisyear, {'vehicle_name','year','firmid'});

        %the data table is useful for going back to look at which car is which
        %so we keep it, but we also need the numerical data as a matrix for our
        %calculations
        carmatrix = table2array(cardatathisyearcharacteristics);
        
        %separate out inputs
        alpha = c(5);
        beta = c(1:4);
        
        %cache the denominator
        denom = 1;
        p = carmatrix(:,5);
        s = carmatrix(:,6);
        X = carmatrix(:,1:4);
        for j = 1:length(s)
            denom = denom + exp(dot(beta, X(j,:)) - (p(j) * alpha));
        end
        
        %compute the share
        share = zeros(length(s));
        for j = 1:length(s)
            share(j) = exp(dot(beta, X(j,:)) - (p(j) * alpha)) / denom;
        end
        
        %Compute the product
        for j = 1:length(s)
            L = L * (share(j)^(s(j)));
        end
    end
    
    %switch to negative so we are trying to find a min instead of a max
    L = -L;
end