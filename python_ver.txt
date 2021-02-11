# -*- coding: utf-8 -*-
"""
Created on Thu Feb 11 11:19:46 2021

@author: clayd
"""

from pulp import *

def optimize():
    prob = LpProblem("Profit", LpMaximize) # max problem
    
    hfc = LpVariable("hfc", lowBound = 0) 
    lfc = LpVariable("lfc", lowBound = 0)
    hfcr = LpVariable("hfcr", lowBound = 0)
    lfcr = LpVariable("lfcr", lowBound = 0)
    hfco = LpVariable("hfco", lowBound = 0)
    lfco = LpVariable("lfco", lowBound = 0)
    ccr = LpVariable("ccr", lowBound = 0)
    cco = LpVariable("cco", lowBound = 0)
    
    # objective function
    prob += -0.4*hfc - 0.4*lfc + 0.3*hfcr + 0.7*lfcr - 0.12*hfco + 0.28*lfco + 1.1*ccr + 0.68*cco
    
    # leq constraints
    prob += hfcr + lfcr + hfco + lfco + ccr + cco <= 3000
    prob += 0.9*hfco + 0.9*lfco + 0.9*cco <= 2000
    prob += hfcr + lfcr + ccr <= 1500
    
    # eq constraints
    prob += (-0.6)*hfc - 0.3*lfc + ccr + cco == 0
    
    #geq constraints
    prob += 0.25*hfco - 0.05*lfco >= 0
    prob += -0.4*hfcr - 0.4*lfcr + 0.6*ccr >= 0
    prob += 0.1*hfcr - 0.2*lfcr >= 0
    prob += -0.2*hfco - 0.2*lfco + 0.8*cco >= 0
    prob += hfcr + lfcr + ccr >= 1000
    prob += 0.9*(hfco + lfco + cco) >= 1000
    
    print(prob)
    
    status = prob.solve()
    LpStatus[status]

    print("max profit is: ", value(prob.objective))
    print("this occurs when ...")
    for var in prob.variables():
        print(f"{var.name}: {var.value()}")

optimize()