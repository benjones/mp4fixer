module remapped.remapped;

/*


  Work with binary data which doesn't use D's layout

  In particular, avoid issues with padding and endianness

 */




auto remapped(Layout)(ubyte[] data){

    import std.traits : Fields, FieldNameTuple, isIntegral;
    import std.meta : staticMap;
    import std.range : only;
    import std.algorithm.iteration: cumulativeFold;
    import std.array;
    import std.bitmanip: peek, write;

    struct Ret {
        private ubyte[] data;

        this(ubyte[] data_){ data = data_; }


        alias fieldNames = FieldNameTuple!Layout;
        alias fields = Fields!Layout;

        enum fieldSize(T) = T.sizeof;
        alias sizes = staticMap!(fieldSize, fields);
        //pragma(msg, sizes);
        //put 0 first since the first field start is at 0
        //we'll never read fieldStarts[N]
        enum fieldStarts = only(0, sizes).cumulativeFold!(function(ulong a, ulong b){
                return a + b;
            })(0UL).array;

        //pragma(msg, fieldStarts);

        //TODO ubyte[] fields?
        static foreach(i, field; Fields!Layout){

            //pragma(msg, field.stringof ~ " " ~ fieldNames[i] ~ " " ~ fieldStarts[i]);
            //TODO: support little endian via an annotation
            auto opDispatch(string f)() const if(f == fieldNames[i]){
                static if(is(field: ubyte[N], ulong N)){
                    //cast to handle char[] without any fuss
                    return cast(field)(data[fieldStarts[i] .. fieldStarts[i] + N][0 .. N]);
                } else {
                    return data[fieldStarts[i] .. $].peek!field;
                }
            }

            //bitmanip.write only supports integral types
            void opDispatch(string f)(field val) if(f == fieldNames[i]){
                static if(is(field: ubyte[N], ulong N)){
                    //cast lets us handle char[] without any fuss
                    data[fieldStarts[i] .. fieldStarts[i] + N] = cast(ubyte[])(val[]);
                } else static if(isIntegral!field){
                    data[fieldStarts[i] .. $].write!field(val, 0);
                } else {
                    static assert(false, "can only write to ubyte[N] or integral types");
                }
            }

        }

        Layout opCast(Layout)(){
            Layout ret;
            static foreach(fieldName; fieldNames){
                __traits(getMember, ret, fieldName) = opDispatch!fieldName;
            }
            return ret;
        }

        void opAssign(const ref Layout rhs){
            static foreach(fieldName; fieldNames){
                opDispatch!fieldName(__traits(getMember, rhs, fieldName));
            }

        }
    }

    return Ret(data);
}


unittest {

    import std.algorithm;
    import std.range;

    struct S1{
        int x;
        ulong y;
        ubyte[4] d;
    }

    ubyte[16] data;
    data[3] = 1;
    auto s1r = remapped!S1(data);
    assert(s1r.x == 1);
    s1r.x = 0x07060504;
    assert(s1r.x == 0x07060504);
    assert(data[0] == 0x07);
    assert(data[1] == 0x06);
    assert(data[2] == 0x05);
    assert(data[3] == 0x04);

    S1 native = cast(S1)s1r;
    assert(native.x == 0x07060504);
    s1r.d = [1,2,3,4];
    assert(s1r.d[].equal([1,2,3,4]));

    S1 other;
    other.x = 1025;
    other.y = 0x7777777777;
    other.d = [8,9,10,11];
    s1r = other;
    assert(s1r.x == 1025);
    assert(s1r.y == 0x7777777777);
    assert(data[3] == 1); //LSB of 1025, big endian layout
    assert(data[12 .. 16].equal([8,9,10,11]));


    struct S3{
        char[4] type;
    }

    ubyte[4] data2;
    auto s3r = remapped!S3(data2);
    s3r.type = "abcd";
    assert(s3r.type == "abcd");
assert(cast(char[])data2 == "abcd");
}
