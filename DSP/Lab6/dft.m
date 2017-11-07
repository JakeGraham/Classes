% compute the DFT of a data vector x using basis functions
function X = dft2(x) 

if size(x,1)<size(x,2)
    x = x';
end


K = length(x)/2 + 1; % number of frequency points to compute
fax = linspace(0,pi,K); % frequency axis in radians
n = (0:length(x)-1)'; % vector of time series indices (starting with 0)

% basis functions
cs = cos(n*fax); % cosine (real) basis functions
ss = sin(n*fax); % sine (imaginary) basis functions

% basis function product with time series
res = cs.*repmat(x,1,K);
ims = ss.*repmat(x,1,K);

X = sum(res) - i*sum(ims);