% Dynamic Programming for Cargo Transfer Optimization

% Data Initialization
numPorts = 6; % Number of ports
demands = [100, 150, 200, 50, 75, 125]; % Demand from each port
distances = [0, 100, 150, 200, 100, 50; % Distance matrix
             100, 0, 75, 125, 150, 100;
             150, 75, 0, 100, 200, 100;
             200, 125, 100, 0, 75, 150;
             100, 150, 200, 75, 0, 125;
             50, 100, 100, 150, 125, 0];
deliveryTimes = [0, 1, 2, 3, 1, 1; % Delivery time matrix
                 1, 0, 2, 2, 3, 2;
                 2, 2, 0, 1, 2, 2;
                 3, 2, 1, 0, 1, 2;
                 1, 3, 2, 1, 0, 1;
                 1, 2, 2, 2, 1, 0];
capacities = [200, 300, 400, 150, 250, 350]; % Capacity of each port
deliveryCosts = [10, 15, 12, 8, 7, 9]; % Cost for each kind of delivery

% Dynamic Programming Algorithm
n = numPorts; % Number of ports
T = max(deliveryTimes(:)) * n; % Total number of time steps
dp = zeros(n, T+1); % Dynamic programming table
path = zeros(n, T+1); % Optimal path table

% Backward Pass
for t = T:-1:1
    for i = 1:n
        if t + deliveryTimes(i, n) <= T+1
            possibleCosts = zeros(n, 1);
            for j = 1:n
                if t + deliveryTimes(i, j) <= T+1
                    possibleCosts(j) = dp(j, t + deliveryTimes(i, j)) + deliveryCosts(j);
                end
            end
            [dp(i, t), path(i, t)] = min(possibleCosts);
        end
    end
end

% Forward Pass to Find Optimal Path
optimalPath = zeros(1, T+1);
[~, optimalPath(1)] = min(dp(:, 1));
for t = 2:T+1
    optimalPath(t) = path(optimalPath(t-1), t-1);
end

% Extract Optimal Path Information
optimalCost = dp(optimalPath(1), 1);
optimalPath = fliplr(optimalPath);

% Display Results
fprintf('Optimal Cost: %.2f\n', optimalCost);
fprintf('Optimal Path: ');
for t = 1:T+1
    fprintf('%d ', optimalPath(t));
end
fprintf('\n');

% Calculate Actual Cargo Transfer
actualCargo = zeros(n, T+1);
actualCargo(round(optimalPath(1)), 1) = demands(round(optimalPath(1)));
for t = 2:T+1
    for i = 1:n
        if t - deliveryTimes(round(optimalPath(t-1)), i) > 0
            actualCargo(i, t) = actualCargo(i, t-1) + actualCargo(round(optimalPath(t-1)), t - deliveryTimes(round(optimalPath(t-1)), i));
        end
    end
end

% Display Actual Cargo Transfer
fprintf('\nActual Cargo Transfer:\n');
for t = 1:T+1
    fprintf('Time Step %d:\n', t-1);
    for i = 1:n
        fprintf('Port %d: %.2f\n', i, actualCargo(i, t));
    end
    fprintf('\n');
end
