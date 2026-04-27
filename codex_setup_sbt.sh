#!/usr/bin/env bash
set -euxo pipefail

echo "===== Codex setup: JDK 17 + sbt for Scala/Chisel ====="

pwd
ls -la

# Install required system packages.
if command -v apt-get >/dev/null 2>&1; then
  apt-get update
  DEBIAN_FRONTEND=noninteractive apt-get install -y openjdk-17-jdk curl ca-certificates gzip
else
  echo "apt-get not found. Please inspect the base image and choose a compatible installation method."
  exit 1
fi

# Install Coursier if missing.
if ! command -v cs >/dev/null 2>&1; then
  curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > /usr/local/bin/cs
  chmod +x /usr/local/bin/cs
fi

# Install sbt into PATH.
cs install sbt --install-dir /usr/local/bin

# Verify tools.
java -version
javac -version
sbt --version
which java
which javac
which sbt

# Locate sbt project.
if [ -f "build.sbt" ]; then
  PROJECT_DIR="$(pwd)"
else
  PROJECT_DIR="$(dirname "$(find . -name build.sbt -print -quit)")"
fi

if [ -z "$PROJECT_DIR" ]; then
  echo "Could not find build.sbt"
  exit 1
fi

echo "Using sbt project directory: $PROJECT_DIR"
cd "$PROJECT_DIR"

# Prefetch dependencies and compile main/test sources.
sbt update
sbt compile
sbt Test/compile

echo "===== Codex setup complete ====="
