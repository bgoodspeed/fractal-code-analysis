function [H,R,cf,S,F, msg]=dfab(D,x,smin,smax,graph,title_text)
% detrends vector a with polynomial of degree D (i.e. for DFA2, D=2)
% dividing the signal in segments of length s
% from smin, e.g. smin=10 (we should have s>D+2, D=highest degree of interp. polynomial)
% to smax, e.g. smax = 300.. 10000 
% (smax should nromally be less than 1/3 of the length of the time series)
% with the specified ratio between successive values of s 
% it has r increasing only 3 times per s interval
% % if graph = 1, shows graph
% OUTPUT:
%           exponent H
%           correlation coeff R for linear regression
%           uncertainty cf for 95% confidence level
%           vector with all used s-values, S
%           vector with all fluctuatioin sizes, F
% Starts by creating the profile of the time series 
% by using its cumulative sum
x=(x-mean(x))/std(x); % data normalization
x=cumsum(x); % x is now the profile of the initial time series

plot(x);
ylabel("cum. sum");
xlabel("sample #");

title(title_text);
print -dpng time_series.png;
ratio=1.2; % ratio by which s is growing
N=length(x);
% F=[]; S=[];
if smin<5
    smin=5;
end
% establish maximum size of resulting matrix:
s=smin; rw=0;
while s<=smax
     s=round(s*ratio);
     rw=rw+1;
end;
sm=zeros(rw,1);
fm=zeros(rw,1);
s=smin; cnt=1;
while s<=smax
    % divide vector in segments of length s, starting with point r
    % windowsnr = number of windows;
    Fn=0; times=0;
    fprintf('.');
    % Fn = sum of squares of differences between original vector and 
    %       interpolation
    % times = how many times differences have been added
    rs=floor(s/3);
    for r=1:rs:s-1
        windowsnr=floor((N-r)/s);
        for i=1:windowsnr
            fp=r+(i-1)*s; lp=r-1+i*s;
            % first point and last point of each window
            ipl=interpolation(x(fp:lp),D);
%            plot(fp:lp,ipl,'k');
            ipld=x(fp:lp)-ipl';
            % difference between original vector and interpolation vector
            Fn=Fn+sum(ipld.^2)/s;
            times=times+1;
        end;
    end;
%    S=[S s];
%    F=[F sqrt(Fn/times)];
    sm(cnt)=s;
    fm(cnt)=sqrt(Fn/times);
    s=round(s*ratio);
    cnt=cnt+1;
end;
S=sm; F=fm;
[H,R,cf, msg]=regconf30(log10(S),log10(F),'k+','b',graph, title_text);
end

function y=interpolation(x,n)
% input: vector x, polynomial degree n
% output: vector y from polynomial interpolation for x
[p,s]=polyfit_nowarnings((1:length(x)),x',n);
y=polyval(p,(1:length(x)));
end

function [p,S,mu] = polyfit_nowarnings(x,y,n)
%POLYFIT Fit polynomial to data.
% REMOVED WARNINGS TO AVOID DELAYS IN DFA
%   P = POLYFIT(X,Y,N) finds the coefficients of a polynomial P(X) of
%   degree N that fits the data Y best in a least-squares sense. P is a
%   row vector of length N+1 containing the polynomial coefficients in
%   descending powers, P(1)*X^N + P(2)*X^(N-1) +...+ P(N)*X + P(N+1).
%
%   [P,S] = POLYFIT(X,Y,N) returns the polynomial coefficients P and a
%   structure S for use with POLYVAL to obtain error estimates for
%   predictions.  S contains fields for the triangular factor (R) from a QR
%   decomposition of the Vandermonde matrix of X, the degrees of freedom
%   (df), and the norm of the residuals (normr).  If the data Y are random,
%   an estimate of the covariance matrix of P is (Rinv*Rinv')*normr^2/df,
%   where Rinv is the inverse of R.
%
%   [P,S,MU] = POLYFIT(X,Y,N) finds the coefficients of a polynomial in
%   XHAT = (X-MU(1))/MU(2) where MU(1) = MEAN(X) and MU(2) = STD(X). This
%   centering and scaling transformation improves the numerical properties
%   of both the polynomial and the fitting algorithm.
%
%   Warning messages result if N is >= length(X), if X has repeated, or
%   nearly repeated, points, or if X might need centering and scaling.
%
%   Class support for inputs X,Y:
%      float: double, single
%
%   See also POLY, POLYVAL, ROOTS.

%   Copyright 1984-2005 The MathWorks, Inc.
%   $Revision: 5.17.4.6 $  $Date: 2005/03/07 17:32:04 $

% The regression problem is formulated in matrix format as:
%
%    y = V*p    or
%
%          3  2
%    y = [x  x  x  1] [p3
%                      p2
%                      p1
%                      p0]
%
% where the vector p contains the coefficients to be found.  For a
% 7th order polynomial, matrix V would be:
%
% V = [x.^7 x.^6 x.^5 x.^4 x.^3 x.^2 x ones(size(x))];

if ~isequal(size(x),size(y))
    error('MATLAB:polyfit:XYSizeMismatch',...
          'X and Y vectors must be the same size.')
end
x = x(:);
y = y(:);
if nargout > 2
   mu = [mean(x); std(x)];
   x = (x - mu(1))/mu(2);
end
% Construct Vandermonde matrix.
V(:,n+1) = ones(length(x),1,class(x));
for j = n:-1:1
   V(:,j) = x.*V(:,j+1);
end
% Solve least squares problem.
[Q,R] = qr(V,0);
ws = warning('off','all'); 
p = R\(Q'*y);    % Same as p = V\y;
warning(ws);
if size(R,2) > size(R,1)
   warning('MATLAB:polyfit:PolyNotUnique', ...
       'Polynomial is not unique; degree >= number of data points.')
elseif condest(R) > 1.0e10
    if nargout > 2
%        warning('MATLAB:polyfit:RepeatedPoints', ...
%            'Polynomial is badly conditioned. Remove repeated data points.')
    else
%        warning('MATLAB:polyfit:RepeatedPointsOrRescale', ...
%            ['Polynomial is badly conditioned. Remove repeated data points\n' ...
%            '         or try centering and scaling as described in HELP POLYFIT.'])
    end
end
r = y - V*p;
p = p.';          % Polynomial coefficients are row vectors by convention.
% S is a structure containing three elements: the triangular factor from a
% QR decomposition of the Vandermonde matrix, the degrees of freedom and
% the norm of the residuals.
S.R = R;
S.df = max(0,length(y) - (n+1));
S.normr = norm(r);
end

function [a,r,cf, msg]=regconf30(x,y,st1,st2,graph, title_text)
% returns linear regression results for vectors x, y
% (unlike regconf.m, it does not divide the exponent by 2)
% if graph=1, it represents the points with st1 and the line with st2
% if graph = 0, no graph
% a = slope
% b = intercept
% r = correlation coefficient
% cf = 95% confidence interval for slope (add to and subtract from a to get interval limits)
% make sure we have line (as opposed to column) vectors:
[qa,qb]=size(x);
if qa>qb 
    x=x'; 
end;
[qa,qb]=size(y);
if qa>qb 
    y=y'; 
end;
% compute slope and its 95% confidence intervals
X = [ones(length(x),1) x(:)];
[b,bint] = regress(y',X);
a=b(2); cf=bint(2,2)-a;
% compure correlation coefficient;
p=polyfit(x,y,1);a=p(1); b=p(2);
c=corr(x,y); % corrcoef is different in octave than matlab
r=c(1,1); % was 1,2 
u=polyval(p,x);
msg=['H = ',num2str(a),' +- ',num2str(cf),' (95% confidence); R=',num2str(r)];
disp(msg);

if graph==1
    % figure; axes('FontSize',20);


    if st2==0
        plot(x,y,st1);
    else 
        plot(x,y,st1,x,u,st2,'Markersize',10);
    end;
    xlabel("log s");
    ylabel("log F(s)");

    title(cstrcat(title_text, "\n",msg));
        
    print -dpng log_log.png;
    grid;
end;
%disp(['H from ',num2str(a-cf),' to ',num2str(a+cf)]);
end
