resource "aws_instance" "nnu_instance" {
  ami                         = "ami-0ee0c841e0940c58f"
  instance_type               = "t2.nano"
  iam_instance_profile        = "ecsInstanceRole"
  associate_public_ip_address = true
  availability_zone           = "ap-northeast-1a"
  subnet_id                   = aws_subnet.nnu_public_subnet.id
  # TODO 別で管理する。ECSの設定をいい感じにすればよいはず
  user_data                   = <<-EOF
    #!/bin/bash
    echo ECS_CLUSTER=nnu-cluster >> /etc/ecs/ecs.config;echo ECS_BACKEND_HOST= >> /etc/ecs/ecs.config;
  EOF
  # TODO "sg-0ade6ecc1ef973c61" を消す
  vpc_security_group_ids      = [aws_security_group.nnu_security_group_allow_all_egress.id, "sg-0ade6ecc1ef973c61"]
  tags = {
    "Name"        = "ECS Instance - EC2ContainerService-nnu-cluster"
    "Description" = "This instance is the part of the Auto Scaling group which was created through ECS Console"
  }
}

resource "aws_ecs_cluster" "nnu_cluster" {
  name = "nnu-cluster"
  setting {
    name  = "containerInsights"
    value = "disabled"
  }
  tags = {
    "Application" = "NNU"
  }
}

resource "aws_ecs_service" "nnu_service_prod" {
  name    = "nnu-prod-service"
  cluster = aws_ecs_cluster.nnu_cluster.id
  desired_count = 1

  enable_ecs_managed_tags           = true
  health_check_grace_period_seconds = 0
  wait_for_steady_state             = true
  ordered_placement_strategy {
    field = "attribute:ecs.availability-zone"
    type  = "spread"
  }
  ordered_placement_strategy {
    field = "instanceId"
    type  = "spread"
  }
  task_definition = "nnu-prod-task:17" # TODO ignoreに入れる
  tags = {
    "Environment" = "Production"
  }
}

# resource "aws_ecs_task_definition" "nnu_task_prod" {
#   family                = "service"
#   container_definitions = file("task-definitions/service.json")
#
#   volume {
#     name      = "service-storage"
#     host_path = "/ecs/service-storage"
#   }
#
#   placement_constraints {
#     type       = "memberOf"
#     expression = "attribute:ecs.availability-zone in [us-west-2a, us-west-2b]"
#   }
# }

# TODO
# * Auto scaling group
#   * t2.nanoにする
#   * Reserved instanceを使う
# * ECS task
# * ECR
# * CodePipeline
# * DynamoDB
