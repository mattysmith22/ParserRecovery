let
    var exit : 0;
    var acc : 0;
    var input : 0
in
begin
    while \exit d
        begin
            getint(input);
            if input == 0 then
                exit := 1
            else
                acc := acc + input;
                printint(acc)
            end
        end
end