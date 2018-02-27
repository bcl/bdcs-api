FROM welder/bdcs-api-build-img:latest

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

COPY dist/build/bdcs-api-server/bdcs-api-server /usr/local/bin
