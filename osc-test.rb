require 'ruby-osc'

include OSC

OSC.run do
  server = Server.new 33333
  client = Client.new 11111

  server.add_pattern /.*/ do |*args|       # this will match any address
    p "/.*/:       #{ args.join(', ') }"
  end
  end
