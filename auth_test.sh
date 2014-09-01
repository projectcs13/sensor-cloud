#! /bin/bash

### Test Variables
MY_ID="104295496712959038073"
OTHER_PUBLIC_ID="108895705188930031602"
OTHER_PRIVATE_ID="107908217220817548513"

MY_STREAM_ID="ZLDDaaZATcK451vMMdHqWw"
RANKED_STREAM="mh5KhW9sTbGbmVRCSItOhg"
MY_ACCESS_TOKEN="ya29.WwDJorUu9ByRvxwAAAADBNA6_9H79kwom8mxZZOdHTvmWkLwwQyj-hV5iR0DPw"



printf "##########################################################\n"
printf "##   OpenID Connect Authentication/Authorization Test  ###\n"
printf "##########################################################\n\n"


printf "####  Rule 1  #### - "
printf "Can MAKE anything with our own data except manipulating datapoints\n"

printf "\n\nShould be OK -> Update my own user info\n"
curl -XPUT "http://localhost:8000/users/$MY_ID" -H "Access-Token: $MY_ACCESS_TOKEN" -H "Content-Type: application/json" -d '{"description": "testing PUT with my own user"}'

printf "\n\nShould be OK -> Get my updated user info\n"
curl -XGET "http://localhost:8000/users/$MY_ID" -H "Access-Token: $MY_ACCESS_TOKEN"

printf "\n\nShould FAIL -> CAN NOT create datapoints\n"
curl -XPOST "http://localhost:8000/streams/$MY_STREAM_ID/data" -H "Access-Token: ya29.WgBV0eqjpIxktSEAAABq5sHdJVSN64Ij3ztnhD49PW6-oEyLJ7M7F3CGAxkKeEvx1Y8Quj-SuVSavRhNmFc" -H "Content-Type: application/json" -d '{"data":[{"timestamp":"2013-12-04T17:43:02.000","value":117.96},{"timestamp":"2013-12-04T17:47:02.000","value":117.96}]}'

printf "\n\n---------------------------------------------------------------------------\n\n"



printf "####  Rule 2  #### - "
printf "Can NOT MAKE anything to private users\n"

printf "\n\nShould FAIL -> Get user info of the IoT Framework's registered user\n"
curl -XGET "http://localhost:8000/users/$OTHER_PRIVATE_ID" -H "Access-Token: $MY_ACCESS_TOKEN"
printf "\n\nShould FAIL -> Update user info of the IoT Framework's registered user\n"
curl -XPUT "http://localhost:8000/users/$OTHER_PRIVATE_ID" -H "Access-Token: $MY_ACCESS_TOKEN" -H "Content-Type: application/json" -d '{"description": testing PUT with my own user!}'

printf "\n\n---------------------------------------------------------------------------\n\n"



printf "####  Rule 3  #### - "
printf "Can ONLY GET User/S/VS from other public users\n"

printf "\n\nShould be OK -> Fetch public user's data profile\n"
curl -XGET "http://localhost:8000/users/$OTHER_PUBLIC_ID" -H "Access-Token: $MY_ACCESS_TOKEN"

printf "\n\nShould be OK -> Fetch public user's streams\n"
curl -XGET "http://localhost:8000/users/$OTHER_PUBLIC_ID/streams" -H "Access-Token: $MY_ACCESS_TOKEN"

printf "\n\nShould be OK -> Fetch public user's vstreams\n"
curl -XGET "http://localhost:8000/users/$OTHER_PUBLIC_ID/vstreams" -H "Access-Token: $MY_ACCESS_TOKEN"

printf "\n\nShould FAIL -> CAN NOT Fetch public user's triggers\n"
curl -XGET "http://localhost:8000/users/$OTHER_PUBLIC_ID/triggers" -H "Access-Token: $MY_ACCESS_TOKEN"

printf "\n\nShould FAIL -> CAN NOT Update a public user's stream\n"
curl -XPUT "http://localhost:8000/streams/$MY_STREAM_ID" -H "Access-Token: $MY_ACCESS_TOKEN" -H "Content-Type: application/json" -d '{"name": "Now it is my stream"}'

printf "\n\n---------------------------------------------------------------------------\n\n"



printf "####  Rule 4  #### - "
printf "Can PUT the ranking of other user's stream\n"

printf "\n\nShould be OK -> Update the ranking of a stream, either if it is ours or not\n"
curl -XPUT "http://localhost:8000/streams/$RANKED_STREAM/_rank" -H "Access-Token: $MY_ACCESS_TOKEN" -H "Content-Type: application/json" -d '{"stream_id":"$MY_STREAM_ID","value":1.0}'

printf "\n\n---------------------------------------------------------------------------\n\n"



printf "####  Rule 5  #### - "
printf "Anything else is forbidden\n"

printf "\n\nShould FAIL -> Remove other user's stream\n"
curl -XDELETE "http://localhost:8000/streams/$MY_STREAM_ID/" -H "Access-Token: $MY_ACCESS_TOKEN"

printf "\n\n---------------------------------------------------------------------------\n\n"



printf "####  Rule 6  #### - "
printf "Can GET or POST to Users / Streams / VStreams collections\n"

printf "\n\nShould be OK -> Create a new stream\n"
curl -XPOST "http://localhost:8000/streams" -H "Access-Token: $MY_ACCESS_TOKEN" -H "Content-Type: application/json" -d '{"name": "Stream Test 123", "description": "my new stream", "user_id": "$MY_ID"}'

printf "\n\n---------------------------------------------------------------------------\n\n"

printf "All tests done :)\n"
