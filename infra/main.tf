
provider "aws" {
  region = "us-east-1"
}

resource "aws_default_vpc" "default" {
  tags = {
    Name = "Default VPC"
  }
}

resource "aws_default_security_group" "default" {
  vpc_id = aws_default_vpc.default.id

  ingress {
    protocol  = -1
    self      = true
    from_port = 0
    to_port   = 0
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_default_subnet" "default" {
  availability_zone = aws_db_instance.default.availability_zone

  tags = {
    Name = "Default subnet for us-east-1"
  }
}

resource "aws_default_route_table" "default" {
  default_route_table_id = aws_default_vpc.default.default_route_table_id
}

# ACCESS DYNAMO FROM MAIN VPC

resource "aws_vpc_endpoint" "dynamodb" {
  vpc_id       = aws_default_vpc.default.id
  service_name = "com.amazonaws.us-east-1.dynamodb" # TODO: region as param
}

resource "aws_vpc_endpoint_route_table_association" "dynamodb" {
  vpc_endpoint_id = aws_vpc_endpoint.dynamodb.id
  route_table_id  = aws_default_route_table.default.id
}

# ACCESS S3 FROM MAIN VPC

resource "aws_vpc_endpoint" "s3" {
  vpc_id       = aws_default_vpc.default.id
  service_name = "com.amazonaws.us-east-1.s3" # TODO: region as param
}

resource "aws_vpc_endpoint_route_table_association" "s3" {
  vpc_endpoint_id = aws_vpc_endpoint.s3.id
  route_table_id  = aws_default_route_table.default.id
}

# ACCESS SQS FROM MAIN VPC

resource "aws_vpc_endpoint" "sqs" {
  vpc_id            = aws_default_vpc.default.id
  service_name      = "com.amazonaws.us-east-1.sqs" # TODO: region as param
  vpc_endpoint_type = "Interface"
  security_group_ids = [
    aws_default_security_group.default.id,
  ]
  subnet_ids = [aws_default_subnet.default.id]

  private_dns_enabled = true
}

