navigate_to(State, X, Y, ActionList,Distance, DepthLimit) :- 
    
    State = [AgentDict, ObjectDict, _],
    get_dict(x, AgentDict, Ox),
    get_dict(y, AgentDict, Oy),
    write(Ox),
    write(Oy),
           
    (Distance > 0 -> 
        ( X>Ox ->  ( Difference is X-Ox,
                    right(Difference, Lx))
        ;
        X<Ox  ->   (Difference is Ox-Ox,
                    left(Difference, Lx))
        ;
        Y>Oy ->    Difference is Y-Oy,
                    down(Difference, Ly)
        ;
        Y<Oy ->    Difference is Oy-Y,
                up(Difference, Ly) 
            
               )   
                                          
    ),                                
    

    append(Lx,Ly,ActionList),
    write(ActionList).