[<EntryPoint>]
let main argv = 
    let sqlQuery = "
        SELECT a.Id, b.Name
        FROM Table1 as a
        INNER JOIN Table2 b ON a.Id = b.Id
        WHERE a.Name = \"ABC\" AND b.SomeCol = true
    "
    printfn "%A" argv
    0 
