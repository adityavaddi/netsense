MATCH (sensity_admin) WHERE sensity_admin.rolename="sensity_admin"
MATCH (sensity_read_only) WHERE sensity_read_only.rolename="sensity_read_only"
MATCH (sensity_user) WHERE sensity_user.rolename="sensity_user"
SET sensity_admin:Role:Admin, sensity_read_only:Role:Admin, sensity_user:Role:Admin
