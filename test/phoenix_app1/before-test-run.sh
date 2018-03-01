#!/bin/bash
mix do ecto.drop --force, ecto.create --force, ecto.migrate --force
