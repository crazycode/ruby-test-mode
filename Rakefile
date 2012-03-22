# -*- mode: ruby -*-

$PROJECT_NAME = "ruby-test-mode"
$PROJECT_URL = "https://florianebeling.com/var/svn/#{$PROJECT_NAME}/trunk"
$WORK_DIR = 'work'
$RELEASE_DIR = "/var/www/florianebeling.com/"

task :tar do
  sh("svn export #{$PROJECT_URL} #{$WORK_DIR}")
  load("#{$WORK_DIR}/VERSION.rb")
  release_name = "#{PROJECT_NAME}-#{RubyTest::VERSION}"
  mv($WORK_DIR, release_name)
  sh("tar cvzf #{release_name}.tar.gz #{release_name}")
  rm_rf(release_name)
end
