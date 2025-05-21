%% Main Code

%% Reading Dataset
observed_data = readtable('Observed_Activity_Dataset.csv');
time_cost_data = readtable('Travel_Time_and_Cost_Matrix.csv');

%% General Setup
% Number of Observations
N = 2;

% Car Availability (delta_car_n[2] = car availability for person 2)
person_ids = unique(observed_data.PersonID);
delta_car_n = containers.Map('KeyType', class(person_ids(1)), 'ValueType', 'int32');
for i = 1:length(person_ids)
    pid = person_ids(i);
    rows = observed_data.PersonID == pid;
    used_car = any(strcmp(observed_data.MODE(rows), 'car'));
    delta_car_n(pid) = int32(used_car);
end

% General Case
delta_car_value = 1; % The general case should be solved two times (assuming car availability and not availability)

% Activity Duration Set (tau)
tau_set = [0, 1];

% Mandatory Activity Set (h)
h_set = [0, 1];

% Decision stage index (k)
k_set = 0:36;


%% Shop Setting
% Number of Size Attributes for Shopping
S_shop = 2;

%% Time Setting
% Time Grid
t_set = 0:10:360;

% Time Step Size
delta_t = 10;

% Time After which Starting Work is Allowed
t_work_start_after = 60;


% Time Before which Starting Work Must Happen
t_work_start_before = 180;

%% Location Setting
% Set of All Possible Locations
l_set = 1:6;

% Set of All Possible Destination Locations
d_tilde_set = 1:6;

%% Shop Attributes
% Define the Shop Attributes
x_pls = table(...
    repmat("shop", 4, 1), ...
    [5; 5; 6; 6], ...
    [1; 2; 1; 2], ...
    [3000; 120; 2000; 130], ...
    'VariableNames', {'purpose', 'location', 'attribute', 'value'});

%% Activity Purpose
% Activity Purpose Set
p_set = ["home", "work", "shop"];

% Next Activity Purpose
p_tilde_set = ["home", "work", "shop"];

%% Transportation Mode
% Current Mode Set
m_set = ["stay", "car", "walk"];


% Next Mode Set
m_tilde_set = ["stay", "car", "walk"];

%% States
state_template = struct( ...
    't', NaN, ...
    'l', NaN, ...
    'p', NaN, ...
    'tau', NaN, ...
    'm', NaN, ...
    'h', NaN);

%% Actions
action_template = struct( ...
    'd_tilde', NaN, ...
    'm_tilde', NaN, ...
    'p_tilde', NaN);

%% Conditional Choice Set
% Location (L_act_n_list('1').shop)
L_act_n_list = containers.Map('KeyType', class(person_ids(1)), 'ValueType', 'any');

for i = 1:length(person_ids)
    pid = person_ids(i);
    person_rows = observed_data.PersonID == pid;
    person_data = observed_data(person_rows, :);
    
    activities = struct();
    activities.shop = unique(person_data.DEST(strcmp(person_data.ACT, "shop")));
    activities.work = unique(person_data.DEST(strcmp(person_data.ACT, "work")));
    activities.home = unique(person_data.DEST(strcmp(person_data.ACT, "home")));
    
    L_act_n_list(pid) = activities;
end

%{
all_keys = keys(L_act_n_list);

for i = 1:length(all_keys)
    pid = all_keys{i};
    activity_struct = L_act_n_list(pid);
    
    fprintf("Person ID: %s\n", string(pid));
    disp(activity_struct);
end
%}

%% Initialization
% Mode Constant
theta_mode_constant = struct('car', 0, 'walk', 0); % theta_car is base case, should not be estimated, and theta_walk is a free parameter (initial guess, will be updated later)

% Mode Travel Time
theta_TT_mode = struct('car', 0, 'walk', 0); % Both are free parameter (initial guess, will be updated later)

% Mode Travel Cost
theta_cost = 0; % Free parameter (initial guess, will be updated later)

% Same Zone Walking
theta_sz_walk = 0; % Free parameter (initial guess, will be updated later)

% Shopping
theta_C_shop = 0;  % Free parameter (initial guess, will be updated later)
theta_size_shop = 0; % Free parameter (initial guess, will be updated later)
theta_shop_s = struct('s1', 0, 's2', 0); % Both are free parameter (initial guess, will be updated later)
theta_t_shop = 0; % Free parameter (initial guess, will be updated later)

% Working (Defined every 60 minutes and for in-between times, interpolation is used)
theta_work_t = struct('s60', 0, 's120', 0, 's180', 0); % 60 and 180 are free parameters (initial guess, will be updated later), but 120 is base case, should not be estimated

% Home (Defined every 60 minutes and for in-between times, interpolation is used) 
theta_home_t = struct('s0', 0, 's60', 0, 's120', 0, 's180', 0, 's240', 0,...
                     's300', 0, 's360', 0); % Free parameter (initial guess, will be updated later)
