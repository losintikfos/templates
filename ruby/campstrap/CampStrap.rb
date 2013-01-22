Camping.goes :CampStrap

module CampStrap::Controllers

   class Static < R '/static/(.+)'

      TYPES = { '.css' => 'text/css',
                '.js'  => 'text/javascript',
                '.jpg' => 'image/jpeg',
                '.png' => 'image/png' }

      PATH = File.expand_path(File.dirname(__FILE__))

      def get(path)
         @headers['Content-Type'] = TYPES[path[/.\w+$/, 0]] || 'text/plain'
         unless path.include? ".."
            @headers['X-Sendfile'] = "#{PATH}/static/#{path}"
         else
            @status = 403
            "403 - Invalid path"
         end
      end

   end

   class Index < R '/'

      def get
         render :index
      end

   end

end

module CampStrap::Views

   def layout

      html do
         head do
            meta(charset: 'utf-8')
            title { "Bootstrap gone Camping" }
            meta(name: 'viewport', content: 'width=device-width, initial-scale=1.0')
            meta(name: 'description', content: '')
            meta(name: 'author', content: '')
            link(rel: 'stylesheet', type: 'text/css', href: '/static/css/bootstrap.min.css')
            style do
               <<-EOF
body { padding-top: 60px; }
               EOF
            end
            link(rel: 'stylesheet', type: 'text/css', href: '/static/css/bootstrap-responsive.min.css')
            # link(rel: 'shortcut icon', href: '')
         end
         body do
            div(class: 'navbar navbar-inverse navbar-fixed-top') do
               <<-EOF
      <div class="navbar-inner">
        <div class="container">
          <a class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
          </a>
          <a class="brand" href="#">Project name</a>
          <div class="nav-collapse collapse">
            <ul class="nav">
              <li class="active"><a href="#">Home</a></li>
              <li><a href="#about">About</a></li>
              <li><a href="#contact">Contact</a></li>
            </ul>
          </div>
        </div>
      </div>
            EOF
            end

            div(class: 'container') { self << yield }
            script(src: '//code.jquery.com/jquery-latest.js')
            script(src: '/static/js/bootstrap.min.js')

         end
      end

   end

   def index

      p { "Bootstrap gone camping!" }

   end

end
