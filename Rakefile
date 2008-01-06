# -*- mode: ruby -*-

PROJECT_URL = 'https://florianebeling.com/var/svn/ruby-test/trunk'
WORK_DIR = 'ruby-test-release'

task :tar do
  sh "svn export #{PROJECT_URL} #{WORK_DIR}"
  load "#{WORK_DIR}/VERSION.rb"
  release_name = "ruby-test-#{RUBY_TEST_VERSION}"
  mv WORK_DIR release_name
  sh "tar cvzf #{release_name}.tar.gz #{release_name}"
end
