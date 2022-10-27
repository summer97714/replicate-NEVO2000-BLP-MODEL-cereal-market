
function [symS] =Sjrsym(j,k,alpha,pricej,pricek,sharej,sharek)
%syms pricej pricek sharej sharek S
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here
    if j==k 
        %-alpha*pricej=fitted y
    symS= -alpha*pricej*(1-sharej)*(sharej/pricek)
    elseif j ~= k
    symS=(alpha*pricek*sharek)*(sharej/pricek)
    end   
end 

