# GETTERS ####

## missing cases ####

test_that('Not found exprObj returns error', {
    # create test object
    giotto_object = giotto()
    
    expect_error(
        getExpression(giotto_object, spat_unit = 'none', 
                      feat_type = 'none', values = 'raw')
    )
})


