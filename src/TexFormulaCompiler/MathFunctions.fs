module MathFunctions

open System

let a = [
            6.65e+4; -3.61e+4; -3.14e+4; 866.97
            629.33; -379.8; 24.77; -1.716
        ]
        
let b = [
            -1.15e+5; -1.35e+5; 4.76e+3; 2.25e+4;
            -3107.8; -1015.2; 315.35; -30.84
        ]

// https://en.wikipedia.org/wiki/Lanczos_approximation
let rec gamma (t: float) : float = 
    let p = [
                676.5203681218851; -1259.1392167224028; 771.32342877765313;
                -176.61502916214059; 12.507343278686905; -0.13857109526572012;
                9.9843695780195716e-6; 1.5056327351493116e-7
            ]
    let pi = Math.PI
    let sin x = Math.Sin(x)
    let powN (x, n) = Math.Pow(x, n)

    match t with
        | t when t < 0.5 -> pi/(sin(pi * t) * gamma(1. - t))
        | t -> 
            let t = t - 1.
            let g = t + 7.5
            let ep = 0.99999999999980993
            let Ag = List.fold (fun (s, i) pCoeff -> (s + pCoeff/(t + i), i+1.)) (ep, 1.) p |> fst
            sqrt(2.*pi) * powN(g, t + 0.5) * Math.Exp(-g) * Ag

let fact (t: float) : float = 
    let factInt (t: int) : int = 
        let rec factInternal c state = 
            match c with
                | 1 -> state
                | 0 -> 1
                | x -> factInternal (x-1) (state*x)
        
        factInternal t 1

    let truncatedT = Math.Truncate(t)
    let canBeNatural = Math.Sign(t) <> -1 && (t - truncatedT) < Double.Epsilon
    
    if canBeNatural then float(factInt(int(truncatedT)))
    else gamma (t)



