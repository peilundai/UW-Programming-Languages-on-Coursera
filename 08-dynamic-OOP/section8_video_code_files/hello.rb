# Coursera Programming Languages, Part 3, exercise
# Author: Peilun Dai peilun.dai@gmail.com 
# 2020-01-17 


class Hello  # must be capital
    def hello_world ()
        puts "hello, world"
    end 

end 



class A 

    def m1
        puts "hello, world"
    end 

    def m2 (x, y)
        z = 7
        if x > y 
            false 
        else 
            x + y * z
        end 
    end 

end 


a = A.new # if no argument, no need for parenthesis 
a.m1



class B 

    def m1 
        z = 7 
        4
        z += 3
    end 

    def m3 x 
        x.abs * 2 + m1 
    end 

    def m4 
        puts "Hello, world!!!"
        self # return a reference to self object, the current object
    end 

end 


c3 = B.new 
c3.m4 



