clc, clearvars
format rat

% create matrix A for n = 4
% A is a tridiagonal matrix where on main diagonal it's all 2's
% and above below the 2's are -1's

n = 4;                      % size of matrix
a = -1 * ones(n-1,1);       % subdiagonal
b =  2 * ones(n,1);         % main diagonal
c = -1 * ones(n-1,1);       % superdiagonal
A = diag(b) + diag(a,-1) + diag(c,1);

% b vector
b = zeros(n,1);
b(1) = 1;
b(end) = 1;

%% eigenvalues
%{
% Different n values to compare
n_values = [5, 10, 20, 50];
figure; hold on;
for i = 1:length(n_values)
   n = n_values(i);
   k = 1:n;
   lambda = 2 - 2*cos(k*pi/(n+1));  % eigenvalues formula
   plot(k, lambda, 'o-', 'DisplayName', ['n=' num2str(n)]);
end
xlabel('Eigenvalue index k');
ylabel('\lambda_k');
title('Eigenvalues of Tridiagonal Matrix T_n');
legend('show');
grid on;
%}

%% Topelitz matrix
T = toeplitz(rand(1,n));

%% Gaussian elimination
function[] = GE(A, b)
% LU factorization w/o pivoting (A = LU)
% A is nxn and has LU factorization

% copy of original matrix
Aog = A;
bog = b;
[n, n] = size(A);

for k = 1:n-1

    % lij; pivot entry
    A(k+1:n, k) = A(k+1:n, k) / A(k,k);
    bm = A(k+1:n, k);
   
    % make all entries below zero
    A(k+1:n, k+1:n) = A(k+1:n, k+1:n) - A(k+1:n, k) * A(k,k+1:n);
    b(k+1:n) = b(k+1:n) - bm .* b(k);

end
L = eye(n, n) + tril(A, -1);
U = triu(A);

fprintf("With Gaussian Elimination the matrix A is: \n")
disp(U);

%back substitution
% Ax = b; Ux = b -> x = U^-1 *b
x = zeros(n,1);

% backward substitution
for i = n:-1:1
   x(i) = (b(i) - U(i,i+1:n) * x(i+1:n)) / U(i,i);
end

% output
fprintf("With back substitution with the matrix A and vector where \n")
fprintf("A is: \n")
disp(U);
fprintf("and b is: \n")
disp(b);
fprintf("The solution vector to Ax = b, for x is: \n")
disp(x);
format long

%check: Ax = b or b - Ax ~= 0
disp(bog - Aog*x);

end
%GE(A,b)

%% LU factorization without partial pivoting
function[L, U] = LU(A)
% LU factorization w/o pivoting (A = LU)
% A is nxn and has LU factorization

% original A
Aog = A;
[n, n] = size(A);

for k = 1:n-1

    % lij; pivot entry
    A(k+1:n, k) = A(k+1:n, k) / A(k,k);
   
    % make all entries below zero
    A(k+1:n, k+1:n) = A(k+1:n, k+1:n) - A(k+1:n, k) * A(k,k+1:n);

end
L = eye(n, n) + tril(A, -1);
U = triu(A);

% change format
format rat
fprintf("With LU factorization of A, \n")
fprintf("the Lower triangular matrix, L is: \n");
disp(L);

fprintf("and the Upper triangular matrix, U is: \n");
disp(U);

fprintf("Check: A - LU: \n")
disp(Aog - L*U)

end
% get outputs
%[L, U] = LU(A);

function[y, x] = sb(L, U , b)
% implement forward and backward subsitituion to solve Ax = b
% sb = substitution (forward then backwards)
% Ax = (LU)x = Ly = b where y = Ux;
% solve Ly = b for y; then Ux = y for x
% do with code from P8

% get origional b vector
bog = b;

% size of b vector
n = size(bog, 1);

% Ly = b; y = L^-1 * b
y = zeros(n,1);

% forward subsitution
for i = 1:n
   y(i) = (b(i) - L(i,1:i-1) * y(1:i-1)) / L(i,i);
end

% Ux = y; x = U^-1 * y
x = zeros(n,1);

% backward substitution
for i = n:-1:1
   x(i) = (y(i) - U(i,i+1:n) * x(i+1:n)) / U(i,i);
end

fprintf("For backward substitution, y is:\n");
disp(y);
fprintf("For forward subsutition,\n");
fprintf("The solution vector x: \n")
disp(x);

% check
format long
fprintf("Check matrix Ax = b; b - Ax ~= 0 \n")
A = L*U;
disp(bog - A*x)

end

% output
%[y, x] = sb(L, U , b)

%% LU factorization with partial pivoting
function[P, L, U] = PLU(A)
% LU factorization with pivoting (PA = LU)
% A is nxn and has LU factorization

% original A and permuation matrix P
Aog = A;
[n, n] = size(A);
P = eye(n);

for k = 1:n-1

    [r, pivot] = max(abs(A(k:n,k)));
    pivot = pivot + k - 1;
    if pivot ~= k
      
       % Swaping Rows
       A([pivot,c], :) = A([c,pivot], :);
       P([pivot,c], :) = P([c,pivot], :);
    
       if k > 1
           L([k pivot], 1:k-1) = L([pivot k], 1:k-1);
       end
   
    end
    % lij; pivot entry
    A(k+1:n, k) = A(k+1:n, k) / A(k,k);
   
    % make all entries below zero
    A(k+1:n, k+1:n) = A(k+1:n, k+1:n) - A(k+1:n, k) * A(k,k+1:n);

end

L = eye(n, n) + tril(A, -1);
U = triu(A);

% change format
format rat
fprintf("With LU factorization of A with partial pivoting, \n")
fprintf("the Lower triangular matrix, L is: \n");
disp(L);

fprintf("and the Upper triangular matrix, U is: \n");
disp(U);

fprintf("Check: PA - LU: \n")
disp(P*Aog - L*U)

end

format rat

% get outputs
[P, L, U] = PLU(A);
%[P, L, U] = PLU(T);

function[y, x] = Psb(P, L, U , b)
% implement forward and backward substitution to solve Ax = b
% sb = substitution (forward then backwards)
% Ax = (LU)x = Ly = Pb where y = Ux;
% solve Ly = Pb for y; then Ux = y for x

% get original b vector
bog = b;
b = P * b;

% size of b vector
n = size(bog, 1);

% Ly = b; y = L^-1 * b
y = zeros(n,1);

% forward substitution
for i = 1:n
 y(i) = (b(i) - L(i,1:i-1) * y(1:i-1)) / L(i,i);
end

% Ux = y; x = U^-1 * y
x = zeros(n,1);

% backward substitution
for i = n:-1:1
    x(i) = (y(i) - U(i,i+1:n) * x(i+1:n)) / U(i,i);
end

fprintf("For backward substitution, y is:\n");
disp(y);

fprintf("For forward subsutition,\n");
fprintf("The solution vector x: \n")
disp(x);

% check
format long
fprintf("Check matrix (LU)x = Pb; Pb - Ax ~= 0 \n")
A = L*U;
disp(P*bog - A*x)

end
% output
[y, x] = Psb(P, L, U, b)
%[y, x] = Psb(P, L, U, b)

%% Iterative Methods
eigenvalues = eigs(A);
eigenvalues;
format short

function Combine(A, b, w)
% Jacobi, Gauss Seidal and SOR method to approximate a solution to Ax = b
% A = matrix (n, n)
% b = solution vector (n x 1)
% w = relaxation parameter
% initialize
n = size(A,1);      % size of the matrix

% initialize D, L and U. A = D - L - U
D = diag(diag(A)); 
L = -tril(A,-1);
U = -triu(A,1);

% Jacobi
% B = Bjac and c = cjac
Bjac = D^-1 * (L + U);
cjac = D^-1 * b;

% Gauss Seidal
% B = Bgs and c = cgs
Bgs = (D - L)^-1 * U;
cgs = (D - L)^-1 * b;

% SOR
% B = Bsor and c = csor
Bsor = (D - w*L)^-1 * ( (1-w)*D + w*U );
csor = w*(D - w*L)^-1 * b;

% initialize x, convergence and norms
xJ = zeros(n,1);
xGS = zeros(n,1);
xSOR = zeros(n,1);
epsilon = 1e-6;
EnormJ = zeros(n,1);
EnormGS = zeros(n,1);
EnormSOR = zeros(n,1);
RnormJ = zeros(n,1);
RnormGS = zeros(n,1);
RnormSOR = zeros(n,1);

% exact matrix and residual
x_exact = A \ b;

% error norm table
fprintf('\nIterative method Error and Residual norms (Exact - estimate) \n')
fprintf(['Iteration|                        Error Norm                    ' ...
  '   |                Residual Error                | \n'])

% method iteration
for k = 1:2000  % set iteration to something (you do it)

    % Jacobi
    % x(k+1) = Bjac*x(k) + cjac
    xNewJ = Bjac * xJ + cjac;
  
    % GS
    % x(k+1) = Bgs*x(k) + cgs
    xNewGS = Bgs * xGS + cgs;
  
    % SOR
    % x(k+1) = Bsor*x(k) + csor
    xNewSOR = Bsor * xSOR + csor;
   
    % calculate Error norms
    EnormJ(k) = norm(x_exact - xNewJ);
    EnormGS(k) = norm(x_exact - xNewGS);
    EnormSOR(k) = norm(x_exact - xNewSOR);
    
    % Residual norm
    RnormJ(k) = norm(b - A*xNewJ);
    RnormGS(k) = norm(b - A*xNewGS);
    RnormSOR(k) = norm(b - A*xNewSOR);
    
    % convergence
    if EnormJ(k) < epsilon
         fprintf('Jacobi converged at iteration %d\n', k);
    break;
  
    end
  
    %{
  if EnormGS(k) < epsilon
     fprintf('Gauss-Seidal converged at iteration %d\n', k);
     break;
  end
  if EnormSOR(k) < epsilon
     fprintf('SOR converged at iteration %d\n', k);
     break;
  end
  %}
 
    fprintf(['k        Jacobi           Gauss-Seidal                 SOR   ' ...
      '          Jacobi            Gauss-Seidal         SOR   \n'])
  
    fprintf(['%d      %0.10f        %0.10f         %0.10f      ' ...
      '%0.10f        %0.10f         %0.10f \n'], ...
      k, EnormJ(k), EnormGS(k), EnormSOR(k), RnormJ(k), RnormGS(k), RnormSOR(k) );
  
    % change after each loop
    xJ = xNewJ;
    xGS = xNewGS;
    xSOR = xNewSOR;
end

% get norm and k values for plot
EnormJ = EnormJ(1:k);
EnormGS = EnormGS(1:k);
EnormSOR = EnormSOR(1:k);
kVals = 1:k;

% plot k vs error norm
figure;
hold on;
plot(kVals, EnormJ, '-ob','LineWidth',1.5);
plot(kVals, EnormGS, '-og','LineWidth',1.5);
plot(kVals, EnormSOR, '-or','LineWidth',1.5);
xlabel('Iteration k');
ylabel('Error norms ||E||');
title(sprintf('Error Convergence (w = %.2f)', w));
grid on;
legend('Jacobi Error Norm', 'Gauss-Seidel Error Norm', 'SOR Error Norm', ...
  'Location', 'northeast');

% Plot k vs residual norm (regular plot)
% Regular plot
figure;
hold on;
plot(kVals, RnormJ, '-oc','LineWidth',1.5);
plot(kVals, RnormGS, '-oy','LineWidth',1.5);
plot(kVals, RnormSOR, '-om','LineWidth',1.5);
xlabel('Iteration k');
ylabel('Residual norms ||R||');
title(sprintf('Residual Convergence (w = %.2f)', w));
grid on;
legend('Jacobi Residual Norm', 'Gauss-Seidel Residual Norm', ...
   'SOR Residual Norm', 'Location', 'northeast');

end
%Combine(A, b, 0.5)
%Combine(A, b, 1.0)
%Combine(A, b, 1.5)
%Combine(T, b, 0.5)
%Combine(T, b, 1.0)
%Combine(T, b, 1.5)
% Combine(A, b, 2)

%% individual methods
function[kVals, Enorm, Rnorm] = JM(A, b)
% Jacobi method to approximate a solution to Ax = b
% A = matrix (n, n)
% b = solution vector (n x 1)

% initialize
n = size(A,1);      % size of the matrix

% initialize D, L and U. A = D - L - U
D = diag(diag(A));   
L = -tril(A,-1);
U = -triu(A,1);

% B = Bjac and c = cjac
Bjac = D^-1 * (L + U);
cjac = D^-1 * b;

% initialize x, convergence and norms
x = zeros(n,1);
epsilon = 1e-10;
Enorm = zeros(n,1);
Rnorm = zeros(n,1);

% exact matrix
x_exact = A \ b;

% Jacobi method
for k = 1:2000  % set iteration to something (you do it)
  
   % x(k+1) = Bjac*x(k) + cjac
   xNew = Bjac * x + cjac;
  
   % residual
   rJ = b - A*xNew;

   % calculate norms
   Enorm(k) = norm(x_exact - xNew);
   Rnorm(k) = norm(rJ);
   
   % convergence
   if Enorm(k) < epsilon
       fprintf('Jacobi converged for n = %d at iteration %d\n', n, k);
       break;
   end
   
   % change after each loop
   x = xNew;

end

% get norm and k values for plot
Enorm = Enorm(1:k);
Rnorm = Rnorm(1:k);
kVals = 1:k;

%{
% Plot k vs residual norm
figure;
hold on;
semilogy(kVals, Enorm, '-ok','LineWidth',1.5);
semilogy(kVals, Rnorm, '-or','LineWidth',1.5);
xlabel('Iteration k');
ylabel('norm ||x||');
title('Jacobi Method Convergence');
legend('Error Norm', 'Residual Norm', ...
  'Location', 'northeast');
grid on;
%}

end

%JM(A, b)
%JM(T,b)

function GS(A, b)
% Gauss Seidal method to approximate a solution to Ax = b
% A = matrix (n, n)
% b = solution vector (n x 1)

% initialize
n = size(A,1);      % size of the matrix

% initalize D, L and U. A = D - L - U
D = diag(diag(A));    
L = -tril(A,-1);
U = -triu(A,1);

% B = Bgs and c = cgs
Bgs = (D - L)^-1 * U;
cgs = (D - L)^-1 * b;

% initialize x, convergence and norms
x = zeros(n,1);
epsilon = 1e-6;
Enorm = zeros(n,1);
Rnorm = zeros(n,1);

% exact matrix
x_exact = A \ b;

% Gauss Seidal method
for k = 1:1000  % set iteration to something (you do it)
   
    % x(k+1) = Bgs*x(k) + cgs
    xNew = Bgs * x + cgs;
   
    % residual
    rGS = b - A*x_exact;
   
    % calculate norms
    Enorm(k) = norm(x_exact - xNew);
    Rnorm(k) = norm(rGS);
   
    % convergence
    if Enorm(k) < epsilon
       fprintf('Converged at iteration %d\n', k);
       break
    end
   
    % change after each loop
    x = xNew;
end

% get norm and k values for plot
Enorm = Enorm(1:k);
Rnorm = Rnorm(1:k);
kVals = 1:k;

% Plot k vs residual norm
figure;
hold on;
semilogy(kVals, Enorm, '-ob','LineWidth',1.5);
semilogy(kVals, Rnorm, '-or','LineWidth',1.5);
xlabel('Iteration k');
ylabel('norm ||E||');
title('Gauss Seidal Method Convergence');
legend('Error Norm', 'Residual Norm', ...
  'Location', 'northeast');
grid on;
end
%GS(A, b)

function SOR(A, b, w)
% SOR method to approximate a solution to Ax = b
% A = matrix (n, n)
% b = solution vector (n x 1)
% initialize
n = size(A,1);      % size of the matrix

% initalize D, L and U. A = D - L - U
D = diag(diag(A));    
L = -tril(A,-1);
U = -triu(A,1);

% B = Bsor and c = csor
Bsor = (D - w*L)^-1 * ( (1-w)*D + w*U );
csor = w*(D - w*L)^-1 * b;

% initialize x, convergence and norms
x = zeros(n,1);
epsilon = 1e-6;
Enorm = zeros(n,1);
Rnorm = zeros(n,1);

% exact matrix
x_exact = A \ b;

% SOR method
for k = 1:1000  % set iteration to something (you do it)
   
    % x(k+1) = Bsor*x(k) + csor
    xNew = Bsor * x + csor;
   
    % residual
    rSOR = b - A*x_exact;
   
    % calculate norms
    Enorm(k) = norm(x_exact - xNew);
    Rnorm(k) = norm(rSOR);
   
    % convergence
    if Enorm(k) < epsilon
       fprintf('Converged at iteration %d\n', k);
       break;
    end
   
    % change after each loop
    x = xNew;

end

% get norm and k values for plot
Enorm = Enorm(1:k);
Rnorm = Rnorm(1:k);
kVals = 1:k;

% Plot k vs residual norm
figure;
hold on;
semilogy(kVals, Enorm, '-og','LineWidth',1.5);
semilogy(kVals, Rnorm, '-or','LineWidth',1.5);
xlabel('Iteration k');
ylabel('norm ||x||');
title(sprintf('SOR Method Convergence (w = %.2f)', w));
legend('Error Norm', 'Residual Norm', ...
  'Location', 'northeast');
grid on;

end

% for SOR Method; 0 <= w < 1; w = 0.5, 1.5
%SOR(A, b, 0.5)
%SOR(A, b, 1.0)
%SOR(A, b, 1.5)


%%
%{
nVals = [4, 10, 50, 100];

for i = 1:length(nVals)
    
    n = nVals(i);  

    % Build tridiagonal matrix
    a = -1 * ones(n-1,1);
    bvec =  2 * ones(n,1);
    c = -1 * ones(n-1,1);
    A = diag(bvec) + diag(a,-1) + diag(c,1);
    
    % b vector
    b = zeros(n,1);
    b(1) = 1;
    b(end) = 1;
  
    % Call Jacobi
    [kVals, Enorm, Rnorm] = JM(A, b);

    % Plot
    figure; 
    hold on;
    semilogy(kVals, Enorm, '-ok','LineWidth',1.5);
    semilogy(kVals, Rnorm, '-or','LineWidth',1.5);
    xlabel('Iteration k');
    ylabel('norm ||x||');
    title(sprintf('Jacobi Method Convergence (n = %d)', n));
    legend('Error Norm', 'Residual Norm', 'Location', 'northeast');
    grid on;

    % Save figure with n in the filename
    filename = sprintf('Jacobi_n%d.png', n);
    saveas(gcf, filename);   % saves as PNG

end
%}

%{
% semilogy plot
figure;
hold on;
semilogy(kVals, EnormJ, '-ob','LineWidth',1.5);
semilogy(kVals, RnormJ, '--b','LineWidth',0.5);
xlabel('Iteration k');
ylabel('Error and Residual norm ||E||');
title('Jacobi Method Convergence');
grid on;
legend('Jacobi Error Norm', 'Jacobi Residual Norm', ...
  'Location', 'northeast');
% semilogy plot
figure;
hold on;
semilogy(kVals, EnormGS, '-og','LineWidth',1.5);
semilogy(kVals, RnormGS, '--g','LineWidth',0.5);
xlabel('Iteration k');
ylabel('Error and Residual norm ||E||');
title('Gauss-Sediel Method Convergence');
grid on;
legend('Gauss-Seidel Error Norm', 'Gauss-Seidel Residual  Norm', ...
  'Location', 'northeast');
% semilogy plot
figure;
hold on;
semilogy(kVals, EnormSOR, '-or','LineWidth',1.5);
semilogy(kVals, RnormSOR, '--r','LineWidth',0.5);
xlabel('Iteration k');
ylabel('Error and Residual norm ||E||');
title('SOR Method Convergence');
grid on;
legend('SOR Error Norm', 'SOR Residual  Norm', 'Location', 'northeast');
% loglog plot
%{
figure;
hold on;
loglog(kVals, EnormJ, '-ob','LineWidth',1.5);
loglog(kVals, EnormGS, '-og','LineWidth',1.5);
loglog(kVals, EnormSOR, '-or','LineWidth',1.5);
loglog(kVals, RnormJ, '-oc','LineWidth',1.5);
loglog(kVals, RnormGS, '-oy','LineWidth',1.5);
loglog(kVals, RnormSOR, '-om','LineWidth',1.5);
xlabel('Iteration k');
ylabel('Error and Residual norm ||E||');
title('Loglog Method Convergence');
grid on;
legend('Jacobi Error Norm', 'Gauss-Seidel Error Norm', 'SOR Error Norm', ...
  'Jacobi Residual Norm', 'Gauss-Seidel Residual  Norm', 'SOR Residual  Norm', ...
  'Location', 'northeast');
%}
%}


