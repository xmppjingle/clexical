-module(clexical_hanger).

-callback hang(Key::binary(), Value::binary()) -> ok|error.