# > increment the data pointer (to point to the next cell to the right).
# < decrement the data pointer (to point to the next cell to the left).
# + increment (increase by one) the byte at the data pointer.
# - decrement (decrease by one) the byte at the data pointer.
# . output the byte at the data pointer.
# , accept one byte of input, storing its value in the byte at the data pointer.
# [ if the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next command, jump it forward to the command after the matching ] command.
# ] if the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching [ command.

prog = IO.read(ARGV.shift)

CMDS = {
  ">" => "ptr += 1",
  "<" => "ptr -= 1",
  "+" => "tape[ptr] ||= 0; tape[ptr] += 1",
  "-" => "tape[ptr] ||= 0; tape[ptr] -= 1",
  "." => "tape[ptr] ||= 0; putc(tape[ptr].chr)",
  "," => "tape[ptr] = $stdin.getbyte"
}

def block(prog)
  comp = []

  cmd = nil
  amt = 0

  i = 0
  while i < prog.length
    c = prog[i]
    if c == '>' or c == '<' or c == '+' or c == '-'
      if cmd == c
        if c == '>' || c == '+'
          amt += 1
        else c == '<' || c == '-'
          amt -= 1
        end
      else
        if cmd == '>' or cmd == '<'
          comp << "ptr += #{amt}"
        elsif cmd == '-' or cmd == '+'
          comp << "tape[ptr] ||= 0; tape[ptr] += #{amt}"
        end
        cmd = c
        if cmd == '>' or cmd == '+'
          amt = 1
        else
          amt = -1
        end
      end
    else
      if cmd and amt != 0
        if cmd == '>' or cmd == '<'
          comp << "ptr += #{amt}"
        else
          comp << "tape[ptr] ||= 0; tape[ptr] += #{amt}"
        end
        cmd = nil
        amt = 0
      end

      if c == '['
        stack = 1
        j = i + 1
        while stack > 0
          if j >= prog.length
            raise "bad brackets"
          elsif prog[j] == '['
            stack += 1
          elsif prog[j] == ']'
            stack -= 1
          end
          j += 1
        end
        comp << "while tape[ptr] and tape[ptr] != 0"
        comp << block(prog[(i + 1)...(j - 1)])
        comp << "end"
        i = j - 1
      elsif c != ']'
        comp << CMDS[c]
      end
    end
    i += 1
  end

  if cmd and amt != 0
    if cmd == '>' or cmd == '<'
      comp << "ptr += #{amt}"
    else
      comp << "tape[ptr] ||= 0; tape[ptr] += #{amt}"
    end
  end

  return comp.join("\n")

end

#tape = {}
#ptr = 0

File.open("bfprog.rb", 'w'){ |f| f.write("tape = {}; ptr = 0\n" + block(prog)) }

#eval(block(prog))
