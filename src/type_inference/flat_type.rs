#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExtType {
    Empty = 0,      // term `()`,
    Pair = 1,       // terms of form `(x, y)`
    Triple = 2,     // terms of form `(x, y, z)`
    OtherList = 3,  // terms of form `(x_i, ...)` and none of the above
    TaggedList = 4, // terms of form `F(x_i, ...)`
    Map = 5,        // terms of form `{k_i = x_i, ...}` or `F{k_i = x_i, ...}`
    Null = 6,       // named nulls
    Double = 7,     // terms of type double
    String = 8,     // terms of type string
    OtherRdf = 9,   // other rdf terms
    Zero = 10,      // term `0`
    Pos = 11,       // term of type int > 0
    Neg = 12,       // term of type int < 0
}
