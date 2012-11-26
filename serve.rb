require 'sinatra'

set :public_folder, File.dirname(__FILE__) + '/dialyzed/'

get '/' do
  send_file File.join(settings.public_folder, 'output')
end
