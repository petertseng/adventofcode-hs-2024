if (darg = ARGV.find { |a| a.to_i != 0 })
  ARGV.delete(darg)
  day = Integer(darg)
else
  require 'date'
  day = Date.today.day
  puts "guessing day = #{day}"
end

daypad = day.to_s.rjust(2, ?0)
name = "#{daypad}_#{ARGV[0] || 'tobenamed'}"

File.open(__dir__ + '/adventofcode.cabal', ?a) { |f|
  f.puts
  f.puts <<~CABAL
  executable #{name}
    import: bin, flags
    build-depends: base, adventofcode2024, aocsplit, array, containers
    main-is:       #{name}.hs
  CABAL
}

[?2, ''].each { |suffix|
  f = "#{__dir__}/bin/00_template#{suffix}.hs"
  break system("sed -e 's/99/#{day}/' #{f} > #{__dir__}/bin/#{name}.hs") if File.exist?(f)
}
