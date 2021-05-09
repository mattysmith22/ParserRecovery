let
    var exit : 0;
    var acc : 0;
    var input : 0
in
    while \exit do
        begin
            getint(input);
            if input = 0 then
                exit := 1
            else begin
                acc := acc + input;
                printint(acc)
            end
        end
end