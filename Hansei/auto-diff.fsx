#r @"C:\Users\cybernetic\source\repos\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
#load @"C:\users\cybernetic\jupyter-notebooks\maths-repl.fsx" 
 
open System
open Hansei.Continuation  
open System.Collections.Generic 

type DualNumberF(p: float, ?d) =
   let mutable dval = defaultArg d 0.
    
   static member inline (+)(a: DualNumberF, b: DualNumberF) = 
       DualNumberF(a.p + b.p, a.d + b.d)
    
   static member inline (+)(a: DualNumberF, b: float) = 
       DualNumberF(a.p + b, a.d)

   static member inline ( * )(a: DualNumberF, b: DualNumberF) =
       DualNumberF(a.p * b.p, a.d * b.p + b.d * a.p)

   static member inline Pow(a: DualNumberF, b: float) =
       DualNumberF(a.p ** b, b * a.p ** (b - 1.0) * a.d)

   member __.p = p
   member __.d = dval
   override __.ToString() = $"({p},{dval})"

let derivative f x = f (DualNumberF(x,1.)) : DualNumberF
  

let df = DualNumberF

df 5. * df 6.

df 6. **  2.

derivative (fun x -> x**3.) 5.
derivative (fun x -> x*x*x) 5. 
derivative (fun x -> x**2. + x) 4. 
derivative (fun x -> x*x + x) 4. 
derivative (fun x -> (x + 1.) ** 2.) 5. 

//////////////////////////////////////////////////////
let rec fac cn cont =
   match cn with
   | 2 -> 
       printfn "----"
       cont 2
   | n -> 
       printfn "%d" n
       fac (n-1) (fun m -> printfn $"m={m}, n={n}"; cont(n*m))

fac 5 id

//////////////////////////////

type DualNumber(p: float, ?d) =
   let mutable dval = defaultArg d 0. 
   member __.p = p 
   member __.d
       with get () = dval
       and set dx = dval <- dx 

   override __.ToString() = $"({p},{dval})"  

let add (a: DualNumber) (b: DualNumber) k =
   let n = DualNumber(a.p + b.p)
   k n 
   a.d <- a.d + n.d
   b.d <- b.d + n.d
   
let mul (a: DualNumber) (b: DualNumber) k =
   let n = DualNumber(a.p * b.p)
   k n 
   a.d <- a.d + b.p * n.d
   b.d <- b.d + a.p * n.d

let pow (a : DualNumber) (b : float) k =
   let n = DualNumber(a.p ** b)
   k n
   a.d <- a.d + b * a.p ** (b - 1.) * n.d
      

let grad f x =
   let dx = DualNumber(x)
   f dx (fun (v: DualNumber) -> v.d <- 1.)
   dx.d
 
//2*x³
let tstexpr x k =  
   fun x2 ->  
       fun x3 -> mul x3 x k
       |> mul x x2 
   |> add x x     

grad tstexpr 2.

//=================== 

let D = DualNumber  

//5*x^2 + 2 * x 
let expr x0 =
   cont {
       let! x = cont {return x0}   
       let! xsq = mul x x
       let! xsq5 = mul (D 5.) xsq
       let! x2 = mul (D 2.) x
       return! (add xsq5 x2) 
   }   

grad expr 2. 
 
//===========================//

let (+.) a b = add a b
let ( *. ) a b = mul a b

let grad2 f x y =
   let dx = DualNumber(x)
   let dy = DualNumber(y)
   f dx dy (fun (v: DualNumber) -> v.d <- 1.)
   dx.d, dy.d        
    
//y + x²*y
let expr2 x0 y0 =
   cont {
       let! x = cont {return x0}   
       let! y = cont {return y0}   
       let! xsq = x *. x 
       let! xsqy = xsq *. y 
       return! (xsqy +. y) 
   }  

grad2 expr2 3. 5.

//==========================================//

type AST =
   | ADD of AST * AST
   | MUL of AST * AST
   | POW of AST * float
   | Val of float
   | Var of string
   static member inline (+)(a: AST, b: AST) = ADD(a, b)
   static member inline (+)(a: AST, b: float) = ADD(a,Val b)
   static member inline (*)(a: AST, b: AST) = MUL(a, b)
   static member inline (*)(a: float, b: AST) = MUL(Val a, b)
   static member inline (*)(a: AST, b: float) = MUL(a, Val b)
   static member inline Pow(a:AST, b:float) = POW(a,b)
         
let rec parseAst (vars: Collections.Generic.IDictionary<string, DualNumber>) tree =
   cont {
       match tree with
       | ADD (a, b) ->
           let! l = parseAst vars a
           let! r = parseAst vars b
           return! l +. r
       | MUL (a, b) ->
           let! l = parseAst vars a
           let! r = parseAst vars b
           return! (l *. r)
       | POW (a, b) ->
           let! l = parseAst vars a 
           return! (pow l b)
       | Var x -> return! cont { return vars.[x] }
       | Val y -> return! cont { return (DualNumber y) }
   }

let reset d =
   for KeyValue (_, v: DualNumber) in d do
       v.d <- 0.

let gradn vars f = 
   f (fun (v: DualNumber) -> v.d <- 1.)
   [for KeyValue(_, v:DualNumber) in vars do v.d]


let x, y, z = Var "x", Var "y", Var "z"
 
let vars0 = dict ["x", D 2.; "y", D 3.]

3. * x ** 2. + 2. * x * y + 5.
|> parseAst vars0 
|> gradn vars0

let vars = dict ["x", D 3.; "y", D 5.]

5. * x * x * y + y 
|> parseAst vars
|> gradn vars 


let vars2 = dict ["x", D 5.] 

x * x * x  
|> parseAst vars2
|> gradn vars2

reset vars2 

parseAst vars2 (x ** 3.)
|> gradn vars2  

reset vars2 

parseAst vars2 ((x + 1.) ** 2.)
|> gradn vars2

reset vars

parseAst vars ((x + 1.) ** 2. * y + y)
|> gradn vars
   

let vars3 = Dictionary(vars)
vars3.Add("z", D 2.)

reset vars3

parseAst vars3 ((2. * (x + y + 1.) + 5. * y) ** 3. * y + y * z ** 2. + (x + y))
|> gradn vars3

#time 
List.map float ["55471"; "273657"; "20"] = [55471.0; 273657.0; 20.0]

[55471.0; 273657.0; 20.0] //add +a.d to pow
[55471.0; 273656.0; 20.0] //(remove + a.d)
[55471.0; 273657.0; 20.0]
[55471.0; 194145.0; 20.0] //(remove + b.d)
[55471.0; 194145.0; 20.0]// remove both

let rec whileLoop n body cv = cont { 
    if n = 0 then return cv
    else 
        let! v = body cv
        return! whileLoop (n-1) body v
}


let expr3 x0  =
   cont {
       let! x = cont {return x0}   
       let! y = whileLoop 3 (fun x -> x *. x) x 
       return y 
   }  

grad expr3 5. 

let expr4 x0 y0 = cont {
    let! x = cont { return x0 }
    let! y = cont { return y0}

    let! y =
        whileLoop 3 (fun x ->
            cont {
                let! z = x *. x
                return! z +. y
            }) x 
    return y
} 

grad2 expr4 5. 2. 
