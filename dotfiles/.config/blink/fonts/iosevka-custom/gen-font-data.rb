require 'base64'

template = File.read('./font-data.css.tmpl')

['regular', 'bold', 'italic', 'bolditalic'].each do |variant|
  font_data = File.read("./woff2/iosevka-custom-#{variant}.woff2")
  template.sub!("<data-#{variant}>", Base64.strict_encode64(font_data))
end

File.open('./font-data.css', 'w') { |f| f.write template }
