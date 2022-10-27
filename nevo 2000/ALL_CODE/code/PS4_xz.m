% This program computes the random coefficeints of a discrete choice model described 
% in paper "Measuring Market Power in the Ready-to-Eat Cereal Industry," 
% Written by Aviv Nevo, May 1998.
% Modified by Bronwyn Hall, April 2005 for Matlab 7 compatibility

clear all
clearAllMemoizedCaches
clc

global invA ns x1 x2 s_jt IV vfull dfull theta1 theti thetj cdid cdindex

% load data. see description in readme.txt
load('/Users/zhaoxialing/Desktop/ps 4/data/iv.mat')
load('/Users/zhaoxialing/Desktop/ps 4/data/ps2.mat')

IV = [iv(:,2:21) x1(:,2:25)];

ns = 20;       % number of simulated "indviduals" per market %
nmkt = 94;     % number of markets = (# of cities)*(# of quarters)  %
nbrn = 24;     % number of brands per market. if the numebr differs by market% 
               % this requires some "accounting" vector %
               
%% Part 1
% gen y: compute the outside good market share by market
    % gen markets: 94 markets*24 brands
    cdid = kron([1:nmkt]',ones(nbrn,1));
    %index id of each markets
    cdindex = [nbrn:nbrn:nbrn*nmkt]';
    %cumsum: Cumulative sum
    temp = cumsum(s_jt);
    %sum by group
    sum1 = temp(cdindex,:);
    sum1(2:size(sum1,1),:) = diff(sum1);
    %for each items, outmarket share
    outshr = 1.0 - sum1(cdid,:);

    y = log(s_jt) - log(outshr); % y vector: delta, mean utility
%% Model 1: OLS without fixed effect
% gen X matrix
constance=ones(nmkt*nbrn,1); % gen constance vector
X1=[constance x1(:,1)];% X matrix: constance plus price

% calculate coef and se
coef1=(X1'*X1)\X1'*y; % get estimate of price coef
e1=y-X1*coef1;  % gen residual
vcov1=(e1'*e1)/(nmkt*nbrn-2)*inv(X1'*X1); % variance covriance matrix
se1 = sqrt(diag(vcov1));

%% Model 2: OLS with fixed effect
% gen X matrix
    X2=([x1]);
    
% calculate coef and se
coef2=(X2'*X2)\X2'*y; % get estimate of price coef
e2=y-X2*coef2;  % gen residual
vcov2=((e2'*e2)/(nmkt*nbrn-25))*inv(X2'*X2); % variance covriance matrix
se2 = (sqrt(diag(vcov2)));

%% Model 3: IV without fixed effect
% IV
n_inst = 20 ;
IV1 = [constance iv(:,2:(n_inst+1)) ];

% calculate coef and se
X3hat=IV1*inv(IV1'*IV1)*IV1'*X1;
coef3=inv(X3hat'*X3hat)*X3hat'*y;
e3=y-X1*coef3;
vcov3=(e3'*e3)/(nmkt*nbrn-2)*inv(X3hat'*X3hat);
se3 = sqrt(diag(vcov3));

%% Model 4: IV with fixed effect
% IV
n_inst = 20 ;
IV2 = [iv(:,2:(n_inst+1)) x1(:,2:(nbrn+1))];

% calculate coef and se
X4hat=(IV2*inv(IV2'*IV2)*IV2'*X2);
coef4=inv(X4hat'*X4hat)*X4hat'*y;%iv
e4=y-X2*coef4;
vcov4=(e4'*e4)/(nmkt*nbrn-25)*inv(X4hat'*X4hat);
se4 = sqrt(diag(vcov4));

mval_t = X2*coef4;         % Fitted log shares
mvalold = exp(mval_t); % Compute shares

mvalold=mval_t/(1+sum(mval_t));

save mval mval_t

save result1.mat coef1 se1 coef2 se2 coef3 se3 coef4 se4               

%% PART II 
%%% let's create the ownership matrix: the ownership matrix is made of
%%% omega star and the Sjr which is a function of the elasticities. 

% So omega star needs to be found 

mat= zeros(25,25);
id14=zeros(2256,1);
for i=1:2256
ids= num2str(id(i));
id14(i)=str2double(ids(1:4));
end 
%unique brand
id14= unique(id14);

%ownership matrix
mat(2:25,1)=id14;
mat(1,2:25)=id14;

for i=2:25   
    matis= num2str(mat(i,1));
    for j=2:25 
    matjs= num2str(mat(1,j));
       if str2double(matis(1))== str2double(matjs(1))
       mat(i,j)=1;
       end
    end 
end 
ostar= mat(2:25,2:25);
rank(ostar)

% let's compute Sjr 
mat(2:25,2:25)=0;
%elasticity of each brand per market
Smatjr=zeros(24,24);
coef4p= coef4(1);

markup=zeros(24,94);

%t=[0:24:2232];
count=1;
for u=0:24:2232
    
   for i=1:24
    for j=1:24
    Smatjr(i,j)= Sjrsym(mat(i+1,1),mat(1,j+1),coef4p,full(x1(u+i,1)),full(x1(u+j,1)),mval_t(u+i,1),mval_t(u+j,1));
    end 
   end
   omega= ostar.*Smatjr;
%rank(Smatjr)rank(omega)
%a1= id(1:24)
%a2= id(25:48)
%a3= id(1129:1152)
%a5=id(2233:2256)
%a= [a1,a2, a3 ,a5]

%%why 1:24 from mval_t
markup(:,count) = inv(omega)*mval_t(1:24,1);

count=count+1;

end 

% Markup:
markupallmarket= reshape(markup,[],1);
meanmarkup = mean(markupallmarket);
stdmarkurp =std(markupallmarket);
medmarkup = median(markupallmarket);

% Marginal Cost:
margcost= -markupallmarket+x1(:,1);
meanmargcost = mean(margcost);
stdmargcost =std(margcost);
medmargcost = median(margcost);

% Margins:
marginallmarket= markupallmarket./x1(:,1);
meanmargin = mean(marginallmarket);
stdmargin =std(marginallmarket);
medmargin = median(marginallmarket);




