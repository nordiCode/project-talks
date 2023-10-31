# Use an official Haskell runtime as a parent image
FROM haskell:9.0

# Set the working directory in the container
WORKDIR /usr/src/app

# Copy the current directory contents into the container at /usr/src/app
COPY . .

# Update only the dependencies without updating the package list
RUN cabal update && cabal install 
RUN cabal build

# Expose the application port
EXPOSE 3000

# Run the application when the container starts
CMD ["cabal", "run"]
