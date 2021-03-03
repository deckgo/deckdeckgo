resource "aws_db_instance" "default" {
  allocated_storage   = 20
  storage_type        = "gp2"
  engine              = "postgres"
  engine_version      = "12.5"
  instance_class      = "db.t2.small"
  name                = "deckdeckgo"
  username            = "deckdeckgo"
  password            = data.external.postgres-password.result.postgres-password
  skip_final_snapshot = true
  allow_major_version_upgrade = true
}

# TODO: terraform-provider-secret
data "external" "postgres-password" {
  program = [
    "${path.module}/script/postgres-password",
  ]
}

output "pguser" {
  value = aws_db_instance.default.username
}

output "pghost" {
  value = aws_db_instance.default.address
}

output "pgport" {
  value = aws_db_instance.default.port
}

output "pgdatabase" {
  value = aws_db_instance.default.name
}

output "pgpassword" {
  value = aws_db_instance.default.password
}

