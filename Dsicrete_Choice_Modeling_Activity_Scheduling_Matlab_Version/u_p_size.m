%% Shop Utility Function

%% Function
function utility_p_size = u_p_size(l, p_tilde, x_pls, theta_shop_s, ...
    theta_C_shop, theta_size_shop)
    % u_p_size: Computes size utility for shop purpose at location l
    % Inputs:
    % - l: location ID
    % - p_tilde: purpose ("shop", "work", etc.)
    % - x_pls: table of shop attributes
    % - theta_shop_s: struct with fields 's1', 's2', ...
    % - theta_C_shop, theta_size_shop: scalars

    if p_tilde ~= "shop"
        utility_p_size = 0;
        return;
    end

    % Filter relevant rows in x_pls
    is_relevant = x_pls.location == l & x_pls.purpose == "shop";
    relevant_x = x_pls(is_relevant, :);

    if isempty(relevant_x)
        utility_p_size = 0;
        return;
    end

    % Compute sum of x_value * exp(theta_shop_s)
    sum_x = 0;
    for i = 1:height(relevant_x)
        attr_num = relevant_x.attribute(i);  % numeric like 1 or 2
        field_name = sprintf('s%d', attr_num);  % e.g., 's1'
        x_value = relevant_x.value(i);

        if isfield(theta_shop_s, field_name)
            theta_s = theta_shop_s.(field_name);
            sum_x = sum_x + x_value * exp(theta_s);
        else
            error("Attribute '%s' not found in theta_shop_s", field_name);
        end
    end

    utility_p_size = theta_C_shop + theta_size_shop * log(sum_x);
end

% utility_p_size = u_p_size(5, "shop", x_pls, theta_shop_s, theta_C_shop, theta_size_shop)