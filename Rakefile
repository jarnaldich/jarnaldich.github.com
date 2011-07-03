desc 'Generate tags pages'
task :tag_cloud do
  puts 'Generating tag cloud...'
  require 'rubygems'
  require 'jekyll'
  include Jekyll::Filters

  options = Jekyll.configuration({})
  site = Jekyll::Site.new(options)
  site.read_posts('')

  html = ''
  max_count = site.tags.map{|t,p| p.size }.max
  site.tags.sort.each do |tag, posts|
    s = posts.size
    font_size = ((20 - 10.0*(max_count-s)/max_count)*2).to_i/2.0
    html << "<a href=\"/tags/#{tag.gsub(/ /,"%20")}\" title=\"Postings tagged #{tag}\" style=\"font-size: #{font_size}px; line-height:#{font_size}px\">#{tag}</a> "
  end
  mkdir_p('_includes')
  File.open('_includes/tags_cloud.html', 'w+') do |file|
    file.puts html
  end
  puts 'Done.'
end

desc 'Generate tags pages'
task :tags  => :tag_cloud do
  puts "Generating tags..."
  require 'rubygems'
  require 'jekyll'
  include Jekyll::Filters

  options = Jekyll.configuration({})
  site = Jekyll::Site.new(options)
  site.read_posts('')

  # Remove tags directory before regenerating
  FileUtils.rm_rf("tags")

  site.tags.sort.each do |tag, posts|
    html = <<-HTML
---
layout: default
title: "tagged: #{tag}"
syntax-highlighting: yes
---
  <h1 class="title">Pages with tagged &ldquo;#{tag}&rdquo;</h1>
   <ul>
   {% for post in site.posts %}
       {% for tag in post.tags %}
           {% if tag == '#{tag}'%}
  <li><span>{{ post.date | date_to_string }}</span>
    &raquo; <a href="{{ post.url }}">{{ post.title }}</a></li>
           {% endif %}
       {% endfor %}
   </ul>
   {% endfor %}
HTML

    FileUtils.mkdir_p("tags/#{tag}")
    File.open("tags/#{tag}/index.html", 'w+') do |file|
      file.puts html
    end
  end
  puts 'Done.'
end  


