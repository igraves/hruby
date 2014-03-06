ActiveRecord::Schema.define(:version => 1) do

  create_table "activities", :force => true do |t|
    t.string   "name"
    t.datetime "start_time"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "shape", :force => true do |t|
    t.integer  "customer_id"
    t.integer  "size"
    t.integer  "width"
    t.integer  "height"
    t.datetime "created_at"
    t.datetime "updated_at"
  end
end
