"""
Marginal Effects
Created on Wed Aug 28 12:27:09 2019
@author: Yuriy
"""
# < https://www.digitalocean.com/community/tutorials/how-to-write-modules-in-python-3 >


# Models: mod_log_best, mod_forest_best
pd.options.display.float_format = '{:,.8f}'.format

# Application of the function
res = marginal_effects_short(mod_forest_best, Data, 'y_bb', ['y_bb', 'y_div', 'tic', 'time'])
res = marginal_effects(mod_log_best, Data, 'y_bb', ['y_bb', 'y_div', 'tic', 'time'], 100, 1)
res = marginal_effects(mod_log_unreg, Data, 'y_bb', ['y_bb0', 'y_div', 'tic', 'time'], 100, 1)

res.to_csv("Marginal effects_logit_unreg.csv")

### Define funcitons:
# data = Data
# model= mod_log_unreg
# y_variable = 'y_bb'
# X_drops = ['y_bb', 'y_div', 'tic', 'time']
def marginal_effects_short(model, data, y_variable, X_drops = []):

        y_eval = data[y_variable]
        X_drops.append(y_variable)
        X_eval = data.drop(X_drops, axis = 1)

        # X_eval.columns
        # Estimate model:
        model_f = model.fit(X_eval, y_eval)

        # Identify dummies:
        dummy_vector = np.zeros(len(X_eval.T))
        binary_fact = X_eval.isin([0, 1]).sum()/X_eval.count()
        dummy_vector[binary_fact==1] = 1
        
        # Calculate marginal effects for each variable:
        # Reference point, prob0:
        X_point = X_eval.mean()
        X_point[dummy_vector==1]=0        
        X_point = X_point.values.reshape(1,-1)
        
        prob0 = model_f.predict_proba(X_point)[0][1]
        
        k = len(X_point.T)
        me_vector = np.zeros(k)

        i = 0
        for x_elem in X_eval.columns:
            # x_elem = X_eval.columns[0]
            print("doing now: ", x_elem)
            X_point = X_eval.mean() 

            if dummy_vector[i] == 1:
                 X_point[x_elem] = 1
            else:
                 X_point[x_elem] += X_eval.std()[x_elem]
                
            X_point = X_point.values.reshape(1, -1)
            prob1 = model_f.predict_proba(X_point)[0][1]
            me = prob1 - prob0
            me_vector[i] = me
            i += 1

        # (3) Prepare output:
        df_output = pd.DataFrame(me_vector, index = X_eval.columns, columns = ['Mg. effects'])
        return(df_output)


def marginal_effects(model, data, y_variable, X_drops = [], k_iter = 10, n_sample = 0.8):
    from scipy.stats import t
    import time
    # Create data-frame to collect me_vectors from experiment
    t_start = time.time()
    # y_eval = data[y_variable]
    X_drops.append(y_variable)
    X_eval = data.drop(X_drops, axis = 1)
    
    # (1) k random samples, loop over k:
    
    results_list = pd.DataFrame(columns = X_eval.columns)

    rs = 12345    
    for counter in range(k_iter):
        # k = 1, k_iter = 10
        
        rs +=1
        data_s = data.sample(frac=n_sample, replace = True, random_state = rs)

        y_eval = data_s[y_variable]
        X_drops.append(y_variable)
        X_eval = data_s.drop(X_drops, axis = 1)

        # Estimate model:
        model_f = model.fit(X_eval, y_eval)

        # Identify dummies:
        dummy_vector = np.zeros(len(X_eval.T))
        binary_fact = X_eval.isin([0, 1]).sum()/X_eval.count()
        dummy_vector[binary_fact==1] = 1
        
        # Calculate marginal effects for each variable:
        # Reference point, prob0:
        X_point = X_eval.mean()
        X_point[dummy_vector==1] = 0        
        X_point = X_point.values.reshape(1,-1)

        prob0 = model_f.predict_proba(X_point)[0][1]
        
        k = len(X_point.T)
        me_vector = np.zeros(k)

        i = 0
        for x_elem in X_eval.columns:
            X_point = X_eval.mean() 

            if dummy_vector[i] == 1:
                 X_point[x_elem] = 1
            else:
                 X_point[x_elem] += X_eval.std()[x_elem]                
            X_point = X_point.values.reshape(1, -1)

            prob1 = model_f.predict_proba(X_point)[0][1]
            me_vector[i] = prob1 - prob0
            i += 1
        iter_res = pd.DataFrame(me_vector.reshape(1,len(me_vector)), columns = X_eval.columns)
        results_list = results_list.append(iter_res)

        t_end = time.time()
        t_diff = t_end - t_start
        print("Iteration ", counter, " in ", k_iter, ". Time passed: ", t_diff, " (seconds)")
        
        # (3) Prepare output:
    n = results_list.count()
    res_mean = results_list.mean()
    res_se = results_list.std()/n**0.5
    res_t = res_mean / res_se
    
    p_val = np.ones(len(X_eval.columns)) - (t.cdf(abs(res_t), n-1))
    df_output = pd.DataFrame(np.zeros([len(X_eval.columns), 3]), index = X_eval.columns, columns = ['Mg. effects', 't-stat', 'p-val'])
    df_output['Mg. effects'] = res_mean
    df_output['t-stat'] = res_t
    df_output['p-val'] = p_val
        # df_output = pd.DataFrame(me_vector, index = X_eval.columns, columns = ['Mg. effects'])
        # return(df_output)

    return(df_output)
    
    
