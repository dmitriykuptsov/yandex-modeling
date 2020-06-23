import re

def RLE(input = ""):
    if len(input) == 0:
        return "";
    if not re.match("^[A-Z]+$", input):
        raise Exception("Invalid input string");
    output = "";
    current_symbol = input[0];
    counter = 1;
    for i in range(1, len(input)):
        if input[i] == current_symbol:
            counter += 1;
        else:
            if counter > 1:
                output += current_symbol + str(counter);
            else:
                output += current_symbol;
            current_symbol = input[i];
            counter = 1;
    if counter > 1:
        output += current_symbol + str(counter);
    else:
        output += current_symbol;
    return output;

def DRLE(input = ""):
    if len(input) == 0:
        return "";
    output = "";
    current_symbol = "";
    accumulator = 0;
    counter = 0;
    while True:
        if re.match("^[A-Z]$", input[counter]):
            accumulator = 0;
            current_symbol = input[counter]
            if counter + 1 == len(input):
                output += current_symbol;
                break;
            if re.match("^[A-Z]$", input[counter + 1]):
                output += current_symbol;
            counter += 1;
            continue;
        if re.match("^[0-9]$", input[counter]):
            accumulator = 10 * accumulator + int(input[counter]);
            if counter + 1 == len(input):
                output += ''.join(([current_symbol] * accumulator));
                break;
            if re.match("^[A-Z]$", input[counter + 1]):
                output += ''.join(([current_symbol] * accumulator));
            counter += 1;
            continue;
        counter += 1;
    return output;

encoded = RLE("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBB");
assert(encoded == "A4B3C2XYZD4E3F3A6B28")
decoded = DRLE(encoded);
assert(decoded == "AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBB");