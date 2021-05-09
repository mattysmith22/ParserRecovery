let
    var n: 0;
    var x: 0;
    var i: 0
in
begin
    getint (n);

    if n < 0
    then x := 0 else x := 1;
    i := 2;
    while \(i > n) do
        begin
            x := x * i;
            i := i + 1
        end;
    printint (x)
end