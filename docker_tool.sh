#!/usr/bin/env bash
while getopts ":h" opt; do
  case ${opt} in
    h )
      echo "Usage:"
      echo "    docker-tool.sh -h        Display this help message."
      echo "    docker-tool.sh build <VERSION>    Builds image"
      echo "    docker-tool.sh test <VERSION>    Tests image"
      echo "    docker-tool.sh publish <VERSION>  Publishes image to Docker Hub"
      echo "    docker-tool.sh latest <VERSION>  Tags images as latest on Docker Hub."
      echo "    docker-tool.sh run <VERSION>  Run the image locally"
      exit 0
      ;;
   \? )
     echo "Invalid Option: -$OPTARG" 1>&2
     exit 1
     ;;
  esac
done


test_docker_image() {
     docker run -d --name "$1" -p 8050:8050 -e HOST='0.0.0.0' berzemus/anon-etaamb:"$1"
     sleep 2
     url=http://localhost:8050/
     status=$(curl --get --location --connect-timeout 5 --write-out %{http_code} --silent --output /dev/null ${url})

     if [[ $status == '200' ]]
     then
      echo "$(tput setaf 2)Image: berzemus/anon-etaamb:${1} - Passed$(tput sgr0)"
      docker kill "$1"
      docker rm "$1"
     else
      echo "$(tput setaf 1)Image: berzemus/anon-etaamb:${1} - Failed$(tput sgr0)"
      docker kill "$1"
      docker rm "$1"
      exit 1
     fi
}

shift $((OPTIND -1))
subcommand=$1; shift
version=$1; shift

case "$subcommand" in
  build)
    docker build -t berzemus/anon-etaamb:${version}  -f ./Dockerfile .
    ;;

  test)
    # Test the images
    test_docker_image ${version}
    ;;

  publish)
    # Push the build images
    docker push berzemus/anon-etaamb:${version}
    # docker push berzemus/anon-etaamb:${version}-cli
    ;;

  latest)
    # Update the latest tags to point to supplied version
    docker tag berzemus/anon-etaamb:${version} berzemus/anon-etaamb:latest
    docker push berzemus/anon-etaamb:latest
    # docker tag berzemus/anon-etaamb:${version}-cli berzemus/anon-etaamb:latest-ocr
    # docker push berzemus/anon-etaamb:latest-ocr
    ;;

  run)
    # Run the image
    docker run -it --rm -p 6006:6006 berzemus/anon-etaamb:${version}
    ;;

esac
