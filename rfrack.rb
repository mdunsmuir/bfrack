CMDS = {
  "." => "putc(tape[ptr].chr)",
  "," => "tape[ptr] = $stdin.getbyte"
}

def block(prog, dep = 0)
  comp = []
  indent = "  " * dep

  mods = {}
  mods.default = 0

  ptr = 0

  rotate_mods = lambda{
    unless mods.size == 0
      mods.each{ |imod, mod|
        next if mod == 0
        str = ["tape["]
        str << "ptr"
        if imod > 0
          str << " + #{imod}"
        elsif imod < 0
          str << " - #{imod.abs}"
        end
        str << "] "
        if mod > 0
          str << "+= "
        else
          str << "-= "
        end
        str << mod.abs.to_s
        comp << indent + str.join
      }
      mods = {}
      mods.default = 0
    end
    if ptr > 0
      comp << indent + "ptr += #{ptr}"
    elsif ptr < 0
      comp << indent + "ptr -= #{ptr.abs}"
    end
    ptr = 0
  }

  i = 0
  while i < prog.length
    c = prog[i]
    if c == '>'
      ptr += 1
    elsif c == '<'
      ptr -= 1
    elsif c == '+'
      mods[ptr] += 1
    elsif c == '-'
      mods[ptr] -= 1
    elsif c == '['
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
      rotate_mods.call
      comp << indent + "ptr += #{ptr}" unless ptr == 0
      ptr = 0
      comp << indent + "while tape[ptr] != 0"
      str = block(prog[(i + 1)...(j - 1)], dep + 1)
      comp << str
      comp << indent + "end"
      i = j - 1
    elsif c != ']' and CMDS[c]
      rotate_mods.call
      comp << indent + CMDS[c]
    end
    i += 1
  end

  rotate_mods.call
  return comp.join("\n")

end

prog = IO.read(ARGV.shift)

File.open("bfprog.rb", 'w'){ |f| f.write("tape = {}\ntape.default = 0\nptr = 0\n" + block(prog)) }

