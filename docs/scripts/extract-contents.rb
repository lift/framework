require 'rubygems'
require 'nokogiri'

Dir.chdir '..' do
  Dir.glob('*.html').each do |html_file|
    contents = File.read(html_file)

    html = Nokogiri::HTML(contents)

    examples =
      html.css('.exampleblock').collect do |example|
        {
          input: example.css('.input code.html').text,
          selector: example.css('.selector code.scala').text,
          output: example.css('.output code.html').text
        }
      end

    puts examples
  end
end
