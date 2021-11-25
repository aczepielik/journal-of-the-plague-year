#!/bin/bash

curl 'https://visuals.guim.co.uk/spreadsheetdata/1B0RfcZT2JoyOY_zlJaO1FQT3M7dEg-XfOaI8iaUwfhs.json' -H 'User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:85.0) Gecko/20100101 Firefox/85.0' -H 'Accept: */*' -H 'Accept-Language: en-US,en;q=0.5' --compressed -H 'Origin: https://interactive.guim.co.uk' -H 'Connection: keep-alive' -H 'Referer: https://interactive.guim.co.uk/2015/jun/plagueMap/' -H 'If-Modified-Since: Thu, 07 Jul 2016 10:30:50 GMT' -H 'If-None-Match: "e6680b151b9346d0577a27c68ed1d293"' -o data_from_guardian.json
