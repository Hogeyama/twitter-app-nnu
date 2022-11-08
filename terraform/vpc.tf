// public subnet一つから成るシンプルなVPC

resource "aws_vpc" "nnu_vpc" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_hostnames = true
  tags = {
    Name = "nnu-vpc"
  }
}

resource "aws_subnet" "nnu_public_subnet" {
  vpc_id                  = aws_vpc.nnu_vpc.id
  cidr_block              = "10.0.0.0/24"
  availability_zone       = "ap-northeast-1a"
  map_public_ip_on_launch = true
  tags = {
    Name = "nnu-public-subnet"
  }
}

resource "aws_internet_gateway" "nnu_internet_gateway" {
  vpc_id = aws_vpc.nnu_vpc.id
  tags = {
    Name = "nnu-internet-gateway"
  }
}

resource "aws_route_table" "nnu_route_table" {
  vpc_id = aws_vpc.nnu_vpc.id
  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.nnu_internet_gateway.id
  }
  tags = {
    Name = "nnu-route-table"
  }
}

resource "aws_route_table_association" "nnu_route_table_association" {
  subnet_id      = aws_subnet.nnu_public_subnet.id
  route_table_id = aws_route_table.nnu_route_table.id
}

resource "aws_security_group" "nnu_security_group_allow_all_egress" {
  // FIXME
  // 既存リソースをインポートしたためname/descriptionが変えられない。
  // 作り直してよくなったタイミングで変える。
  name        = "default"
  description = "default VPC security group"
  vpc_id      = aws_vpc.nnu_vpc.id
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  tags = {
    Name = "nnu-security-group"
  }
}
