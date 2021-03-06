let
    const scaling ~ 1000;
    var error : 2
    var val: 0;
    var lastResult : -1;
    var result : 1000
in
begin
    getint (val)
    if val > 0 then
        begn
            while (result - lastResult > error + lastResult - result > error) do
                begin
                    lastResult := result;
                    result = (result + (val*scaling/result))/2;
                    printint (result)
                end;
            printint (result);
            printint (-1);
        end
    else 
        printint (-1)
end