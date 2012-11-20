require 'sinatra'

set :public_folder, File.dirname(__FILE__) + '/dialyzed/'

get '/' do
  "Hello World!"
end
