#!/bin/bash

mix ecto.drop --force
mix ecto.create --force
mix ecto.migrate --force
