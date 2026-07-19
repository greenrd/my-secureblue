To build this project, run this command:

RUST_BACKTRACE=full bluebuild build ./recipes/recipe.yml

If the build fails at the final manifest step with an error like
`image name "localhost/my-secureblue:latest" is already associated with image ...`
(name already in use), remove the stale tag and rebuild:

podman rmi localhost/my-secureblue:latest
RUST_BACKTRACE=full bluebuild build ./recipes/recipe.yml
