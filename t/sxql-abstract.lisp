(in-package #:cl-user)
(defpackage #:sxql-abstract-test
  (:use #:cl
        #:sxql-abstract
        #:prove))
(in-package #:sxql-abstract-test)

(plan 2)

(subtest "MySQL"
  (with-database-type :mysql
    (let ((sxql:*use-placeholder* nil))
      (subtest "create-table"
        (is (mapcar #'sxql:yield
                    (create-table :enemy
                        ((id :type '(:bigserial () :unsigned)
                             :primary-key t)
                         (name :type '(:varchar 64))
                         (age :type :integer
                              :not-null t)
                         (address :type :text
                                  :not-null nil)
                         (fatal-weakness :type :text
                                         :not-null t
                                         :default "None")
                         (identifying-color :type '(:char 20)))
                      (:unique-key name)
                      (:key age)))
            (list "CREATE TABLE `enemy` (
    `id` BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    `name` VARCHAR(64),
    `age` INTEGER NOT NULL,
    `address` TEXT,
    `fatal_weakness` TEXT NOT NULL DEFAULT 'None',
    `identifying_color` CHAR(20),
    UNIQUE 'unique_enemy_name' ('name'),
    KEY (`age`)
)")
            :test #'equalp))

      (subtest "add-column"
        (is (sxql:yield (sxql:alter-table :tweet
                          (add-column :id :type :bigint :primary-key t :auto-increment t :first t)
                          (add-column :updated-at :type :timestamp)))
            "ALTER TABLE `tweet` ADD COLUMN `id` BIGINT AUTO_INCREMENT PRIMARY KEY FIRST, ADD COLUMN `updated_at` TIMESTAMP"))

      (subtest "drop-column"
        (is (sxql:yield (drop-column :updated-on))
            "DROP COLUMN `updated_on`"))

      (subtest "add-index"
        (is (sxql:yield (add-index :tweet :name "key_tweet_name" :unique t :using :btree))
            "CREATE UNIQUE INDEX `key_tweet_name` USING BTREE ON `tweet` (`name`)"))

      (subtest "drop-index"
        (is (sxql:yield (drop-index :tweet "key_tweet_name"))
            "DROP INDEX `key_tweet_name` ON `tweet`"))

      (subtest "add-primary-key"
        (is (sxql:yield (add-primary-key :tweet (list :id :name)))
            "ALTER TABLE `tweet` ADD PRIMARY KEY (`id`, `name`)"))

      (subtest "drop-primary-key"
        (is (sxql:yield (drop-primary-key :tweet))
            "ALTER TABLE `tweet` DROP PRIMARY KEY")))))

(subtest "PostgreSQL"
  (with-database-type :postgres
    (let ((sxql:*use-placeholder* nil))
      (subtest "create-table"
        (is (mapcar #'sxql:yield
                    (create-table :enemy
                        ((id :type '(:bigserial () :unsigned)
                             :primary-key t)
                         (name :type '(:varchar 64))
                         (age :type :integer
                              :not-null t)
                         (address :type :text
                                  :not-null nil)
                         (fatal-weakness :type :text
                                         :not-null t
                                         :default "None")
                         (identifying-color :type '(:char 20)))
                      (:unique-key name)
                      (:key age)))
            (list
             "CREATE TABLE \"enemy\" (
    \"id\" BIGSERIAL UNSIGNED PRIMARY KEY,
    \"name\" VARCHAR(64),
    \"age\" INTEGER NOT NULL,
    \"address\" TEXT,
    \"fatal_weakness\" TEXT NOT NULL DEFAULT 'None',
    \"identifying_color\" CHAR(20),
    UNIQUE 'unique_enemy_name' ('name')
)"
             "CREATE INDEX \"key_enemy_age\" ON \"enemy\" (\"age\")")
            :test #'equalp))

      (subtest "add-column"
        (is (sxql:yield (sxql:alter-table :tweet
                          (add-column :id :type :bigint :primary-key t :auto-increment t :first t)
                          (add-column :updated-at :type :timestamp)))
            "ALTER TABLE \"tweet\" ADD COLUMN \"id\" BIGSERIAL PRIMARY KEY, ADD COLUMN \"updated_at\" TIMESTAMP"))

      (subtest "drop-column"
        (is (sxql:yield (drop-column :updated-on))
            "DROP COLUMN \"updated_on\""))

      (subtest "add-index"
        (is (sxql:yield (add-index :tweet :name "key_tweet_name" :unique t :using :btree))
            "CREATE UNIQUE INDEX \"key_tweet_name\" USING BTREE ON \"tweet\" (\"name\")"))

      (subtest "drop-index"
        (is (sxql:yield (drop-index :tweet "key_tweet_name"))
            "DROP INDEX \"key_tweet_name\""))

      (subtest "add-primary-key"
        (is (sxql:yield (add-primary-key :tweet (list :id :name)))
            "ALTER TABLE \"tweet\" ADD PRIMARY KEY (\"id\", \"name\")"))

      (subtest "drop-primary-key"
        (is (sxql:yield (drop-primary-key :tweet))
            "ALTER TABLE \"tweet\" DROP PRIMARY KEY")))))

(finalize)
