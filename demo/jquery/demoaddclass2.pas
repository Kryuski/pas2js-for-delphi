uses libjquery;

function greenonred(index : Integer; currentclass:string) : string;

begin
  result:='';
  if (currentClass='red') then
    begin
    result:='green';
    jQuery('p').text('There is one green div');
    end;
end;

begin
  jQuery('div').addClass(@greenonred);
end.