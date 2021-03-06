function [theta, J_history] = gradientDescent(X, y, theta, alpha, num_iters)
%GRADIENTDESCENT Performs gradient descent to learn theta
%   theta = GRADIENTDESENT(X, y, theta, alpha, num_iters) updates theta by 
%   taking num_iters gradient steps with learning rate alpha

% Initialize some useful values
m = length(y); % number of training examples
J_history = zeros(num_iters, 1);

for iter = 1:num_iters

    % ====================== YOUR CODE HERE ======================
    % Instructions: Perform a single gradient step on the parameter vector
    %               theta. 
    %
    % Hint: While debugging, it can be useful to print out the values
    %       of the cost function (computeCost) and gradient here.
    %

   for feat_id = 1:2 
       f(feat_id) = 0;
       for i = 1:m    
            f(feat_id) = f(feat_id) + ( (theta') * ( X(i,:)' )- y(i) ) * X(i,feat_id) ;
       end
       
        
   end
   for feat_id = 1:2
        theta(feat_id) = theta(feat_id) - alpha / m * f(feat_id);
   end





    % ============================================================

    % Save the cost J in every iteration    
    J_history(iter) = computeCost(X, y, theta);

end

end
