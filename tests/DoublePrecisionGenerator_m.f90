module DoublePrecisionGenerator_m
    use Vegetables_m, only: Generator_t

    implicit none
    private

    type, public, extends(Generator_t) :: DoublePrecisionGenerator_t
    contains
        private
        procedure, public :: generate
        procedure, public, nopass :: shrink
    end type DoublePrecisionGenerator_t

    type(DoublePrecisionGenerator_t), public :: DOUBLE_PRECISION_GENERATOR = &
            DoublePrecisionGenerator_t()
contains
    function generate(self) result(random_double)
        use Vegetables_m, only: &
                Generated_t, Generated, getRandomDoublePrecisionWithMagnitude

        class(DoublePrecisionGenerator_t), intent(in) :: self
        type(Generated_t) :: random_double

        associate(a => self)
        end associate
        random_double = Generated(getRandomDoublePrecisionWithMagnitude(1.0d12))
    end function generate

    pure function shrink(value_) result(shrunk)
        use Vegetables_m, only: ShrinkResult_t, ShrunkValue, SimplestValue

        class(*), intent(in) :: value_
        class(ShrinkResult_t), allocatable :: shrunk

        select type (value_)
        type is (double precision)
            if (effectivelyZero(value_)) then
                allocate(shrunk, source = SimplestValue(0.0d0))
            else
                allocate(shrunk, source = ShrunkValue(value_ / 2.0d0))
            end if
        end select
    end function shrink

    pure function effectivelyZero(value_)
        double precision, intent(in) :: value_
        logical :: effectivelyZero

        effectivelyZero = abs(value_) < epsilon(value_)
    end function effectivelyZero
end module DoublePrecisionGenerator_m
