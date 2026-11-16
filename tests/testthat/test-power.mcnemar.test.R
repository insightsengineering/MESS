context("Test power_mcnemar_test")


##
## All of these checks should provide the correct results
##

test_that("Produces validated results", {
    expect_equal(round(power_mcnemar_test(paid=0.05, psi=45/5, power=.8)$n), 22.0)
    expect_equal(round(power_mcnemar_test(paid=0.05, psi=45/5, power=.8, alternative="one.sided")$n), 17.0)
    expect_equal(round(power_mcnemar_test(144, paid=0.25, psi=45/25, method="exact", alternative="one.sided", sig.level=0.025)$power, 2), 0.80)    
})



#test_that("Checking symmetry of results", {
#    expect_equivalent(power_mcnemar_test(paid=0.05, psi=45/5, power=.8), power_mcnemar_test(paid=0.45, psi=5/45, power=.8))
#})
# NOTE on the commented-out symmetry test above: it is not expected to hold.
# psi is documented as p_21/p_12 and must be >= 1 because paid is defined to be
# the *smaller* of the two discordant probabilities. Swapping paid and psi
# (0.05/18 -> 0.45/(1/9)) produces psi < 1, which fails input validation below
# rather than giving a symmetric/equivalent result.
test_that("swapping paid and psi violates the psi>=1 precondition (not symmetric)", {
    expect_error(power_mcnemar_test(paid=0.45, psi=5/45, power=.8),
                 "psi must be 1 or greater")
})

test_that("solving for power given n (method='normal', the default)", {
    result <- power_mcnemar_test(n=60, paid=.1, psi=2, sig.level=0.05, power=NULL)
    expect_equal(result$power, 0.2894199, tolerance=1e-5)
    expect_match(result$method, "approximate")
})

test_that("method='exact' and method='cond.exact' compute a different power than 'normal'", {
    normal_power <- power_mcnemar_test(n=60, paid=.1, psi=2, sig.level=0.05, power=NULL, method="normal")$power

    exact_result <- power_mcnemar_test(n=60, paid=.1, psi=2, sig.level=0.05, power=NULL, method="exact")
    expect_equal(exact_result$power, 0.28216, tolerance=1e-4)
    expect_match(exact_result$method, "exact unconditional")

    cond_exact_result <- power_mcnemar_test(n=60, paid=.1, psi=2, sig.level=0.05, power=NULL, method="cond.exact")
    expect_equal(cond_exact_result$power, 0.2312173, tolerance=1e-4)
    expect_match(cond_exact_result$method, "exact conditional")

    expect_false(isTRUE(all.equal(exact_result$power, normal_power)))
    expect_false(isTRUE(all.equal(cond_exact_result$power, normal_power)))
})

test_that("solving for paid given n, psi, power, sig.level", {
    base_power <- power_mcnemar_test(n=60, paid=.1, psi=2, sig.level=0.05, power=NULL)$power
    result <- power_mcnemar_test(n=60, paid=NULL, psi=2, sig.level=0.05, power=base_power)
    expect_equal(result$paid, 0.1, tolerance=1e-3)
})

test_that("solving for psi given n, paid, power, sig.level", {
    base_power <- power_mcnemar_test(n=60, paid=.1, psi=2, sig.level=0.05, power=NULL)$power
    result <- power_mcnemar_test(n=60, paid=.1, psi=NULL, sig.level=0.05, power=base_power)
    expect_equal(result$psi, 2, tolerance=1e-3)
})

test_that("solving for sig.level given n, paid, psi, power", {
    base_power <- power_mcnemar_test(n=60, paid=.1, psi=2, sig.level=0.05, power=NULL)$power
    result <- power_mcnemar_test(n=60, paid=.1, psi=2, sig.level=NULL, power=base_power)
    expect_equal(result$sig.level, 0.05, tolerance=1e-3)
})

test_that("errors when not exactly one of the mutually exclusive arguments is NULL", {
    expect_error(power_mcnemar_test(n=60, paid=.1, psi=2, sig.level=0.05, power=.8),
                 "exactly one of")
})

test_that("errors for invalid paid and psi values", {
    expect_error(power_mcnemar_test(n=60, paid=.6, psi=2, sig.level=0.05, power=NULL),
                 "paid is the smallest discordant probability")
    expect_error(power_mcnemar_test(n=60, paid=.1, psi=0.5, sig.level=0.05, power=NULL),
                 "psi must be 1 or greater")
    expect_error(power_mcnemar_test(n=60, paid=.4, psi=3, sig.level=0.05, power=NULL),
                 "psi cannot be so big")
})
