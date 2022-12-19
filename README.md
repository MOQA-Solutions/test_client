
# Getting Started

This application represents a proposed solution to the coding test given by Birra.<br>
I have implemented all features mentionned in the text that is sent before.<br>

# Guide

After starting this application which is the application in question, we will have
3 files in the **logging** folder:
- `info_logging.log` it's just an additional file to log information log events as progress reports and others.<br>
- `debug_state_logging.log` which log the most required logs : 
  - If there is no data during one minute
  - If new data arrives
  - Unsuccessful connection attempt
  - Fail of an established connection
  - Additionally, when a **Keep Alive** timer is expired
- `debug_monitor_logging.log` which log a new reestablished connection after fail **if** `monitor` parameter
is set to `true`.<br>
<a/>
As required, the interval between a closed connection and a new one can't be less than 10 seconds to avoid spams.<br>
The `host` and `port` and `monitor` variables are given as environment variables, you can check them
[here]

# Example

```
%% On a separate terminal 
git clone https://github.com/AbdelghaniMohammed/test_client.git
cd test_client
rebar3 shell
%% We start by running the client side without the server side
%% That will result in connection attempt fail and restarting the gen_statem
%% The customized supervisor will control their restarts rate 
%% The gen_statem worker will try to connect once in 10 seconds
%% You can check log reports in debug_state_logging.log
%% Of course we can start the server side first to connect directly when starting the client
%% Now we will start the server side on another terminal from the same machine(host is localhost)
git clone https://github.com/AbdelghaniMohammed/test_server.git
cd test_server
rebar3 shell
%% Now the client will connect to this TCP connection and we can check that in debug_state_logging.log which will not log again the unsuccessful connection report
%% Now we can send data from the server as mentionned before by
test_server:send_data("hello").
%% We can check in debug_state_logging.log the receive of binary data
%% If we don't receive anything from the server within one minute, we will have a log of that in debug_state_logging.log
%% Each 1 minute without receiving anything from the server results in a log 
%% If we don't receive anything from the server within the Keep Alive timer, the connection is considered expired and will be closed
%% When a connection is closed for any reason, the gen_statem will try to reconnect immediately
%% When the monitor parameter is true, and the connection is reestablished after fail, then we will have a separated log in debug_monitor_logging.log

